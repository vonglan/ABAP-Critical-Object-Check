CLASS zcl_s_trkorr_chk_critical_obj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF tp_message
                 , msgid TYPE syst-msgid
                 , msgty TYPE syst-msgty
                 , msgno TYPE syst-msgno
                 , msgv1 TYPE syst-msgv1
                 , msgv2 TYPE syst-msgv2
                 , msgv3 TYPE syst-msgv3
                 , msgv4 TYPE syst-msgv4
                 , END OF tp_message .
    TYPES:
      ttp_message TYPE STANDARD TABLE OF tp_message WITH DEFAULT KEY .

    CLASS-DATA s_with_analysis TYPE abap_bool READ-ONLY .
    CLASS-DATA s_error_threshold_dep_obj TYPE i READ-ONLY .

    METHODS constructor .
    CLASS-METHODS class_constructor .
    CLASS-METHODS check
      IMPORTING
        !i_it_trkorr                   TYPE trkorrs OPTIONAL
        !i_tr_objects                  TYPE tadir_key_t OPTIONAL
        !i_with_analysis               TYPE abap_bool OPTIONAL
        !i_threshold_nr_dependent_objs TYPE i DEFAULT 200
        !i_threshold_duration_min      TYPE i DEFAULT 1
        !i_threshold_nr_errors         TYPE i DEFAULT 5 .
    CLASS-METHODS display_findings .
    CLASS-METHODS store_findings
      IMPORTING
        !i_extnumber TYPE balnrext .
    CLASS-METHODS get_findings_text
      EXPORTING
        !e_has_error TYPE abap_bool
      RETURNING
        VALUE(r)     TYPE stringtab .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: s_messages   TYPE ttp_message,
                s_it_trkorr  TYPE trkorrs,
                s_tr_objects TYPE tadir_key_t.
    CLASS-METHODS get_logger
      IMPORTING
        i_extnumber TYPE balnrext
      RETURNING
        VALUE(r)    TYPE REF TO object.
    CONSTANTS: con_logger_name TYPE seoclsname VALUE 'ZCL_X_LOGGER_FACTORY'.  " Note the standard name is without X_
ENDCLASS.



CLASS zcl_s_trkorr_chk_critical_obj IMPLEMENTATION.


  METHOD check.
    s_it_trkorr = i_it_trkorr.
    s_tr_objects = i_tr_objects.
    s_with_analysis = i_with_analysis.

    lcl_threshold=>init( i_threshold_nr_dependent_objs = i_threshold_nr_dependent_objs
                         i_threshold_duration_min      = i_threshold_duration_min
                         i_threshold_nr_errors         = i_threshold_nr_errors ).

    lcl_main=>check( i_it_trkorr = s_it_trkorr
                     i_tr_objects = s_tr_objects ).
  ENDMETHOD.


  METHOD class_constructor.
    s_error_threshold_dep_obj = lcl_threshold=>con_error_threshold_dep_obj.
  ENDMETHOD.


  METHOD constructor.
*----------------------------------------------------------------------*
*                         D r a e g e r   I T                          *
*----------------------------------------------------------------------*
* Developer Tools / Check transports for critical objects
*----------------------------------------------------------------------*
* Checks transports whether they contain critical objects
* that might lead to dumps when the transport is imported
* to the production system
*
* Test report: ZS_TEST_CRIT_OBJ
*----------------------------------------------------------------------*
* Author: Edo von Glan
* Date          : 2018-11 (zcl_s_trkorr_chk_critical_objs)
* Major Redesign: 2019-08 (zcl_s_trkorr_chk_critical_obj)
*   - Performance (buffering)
*   - Functional extension/generalization (to be able to identify the risks
*       that lead to 2 times 50 import dumps on 2019-05-09 and on 2019-08-27)
*   - Integration of 5 check types into one check (also simplification of customizing)
* Major Change:   2020-01
*   - Distinguish Usage Chain from Generation Chain. For code changes, only Generation Chain is important
*
* (No RFC, because no business functionality, just local and Solution Manager development)
*
* Check Mechanisms:
*
* 1. Main mechanism: Counting dependent objects (users) recursively.
* This check counts recursively the static usage of important object types.
* The number of development objects that need to be re-activated
* - BUGFIX 20200121: or that are users (AND the changed object is DDIC)
* is the criterion for ERRORs.
* In addition, the set of dependent objects discovered here, is used as basis for the other
* two checks.
* Reason:
* These are all the Dumps because a re-activation (generation) of the users (includers) is required.
* The necessity for re-activation is determined by the SAP Kernel recursively
* (A uses B uses C. When C is changed, also A needs to be re-activated), also
* for the static inclusion of code in frame programs.
*
* 2. Identifying "popular" Objects (among the directly changed or dependent objects).
* This check identifies
* - Public Sections of Global Classes
* - Global Interfaces
* - Function Modules
* that were changed (or will be indirectly reactivated because they are dependent objects - see above)
* and are called (used) from code in several other packages.
* In this case, the "dependent objects" metric is not used.
* The number of callers that are in other (not the same and not related)
* packages is a much better indicator of risk in this case, than the simple number of callers.
* And in this case, the threshold values are much lower (e.g. 5 is critical, compared
* to 100 dependent objects for the main mechanism).
* Further recursion (whether the "users" are themselves called by other "users") is not needed in this case,
* because there is no "activation hierarchy" leading to dumps, but only a (non-recursive)
* runtime check.
* Q: Why is this not implemented for PROG (FORMs and local classes)?
* A: for PROG, there is no direct/recommended way to call individual procedures from other programs.
* Also, the external calling of PUBLIC methods of global classes, interfaces and function modules
* is a very frequent cause of import dumps.
*
* 3. Detecting "Critical" Objects (among the directly changed or dependent objects).
* These are objects that are defined in customizing as critical, because they are known to cause many dumps
* when they are directly changed or reactivated due to changes in dependent objects.
*
* Testing notes: To get transport numbers for testing:
*   MT3, table e070
*   trfunction = L
*   trstatus   = R
*   as4date is the import date
*
* Usage notes:
* When you run the check "without threshold", some objects like VBAK have more than 100,000 indirect users.
* On the other hand, if you run "with threshold" (e.g. the default 200), then it can be that critical
* objects are ignored (because they are not in the first 200).
*
* Design Decisions
* Because we want to buffer the check results for objects on all levels of the recursion trees,
* we cannot aggregate the results (for the complete recursion tree of one transport object, while
* it is traversed) in one class instance.
* Instead, the recursively called check method has to aggregate the results for its subtree objects
* in its return parameter. (Having separate instances for each checked object in the tree
* is not an option, for performance reasons.)

* Q: Why aren't BAdI implementations reported as critical/popular/anything?
* A: It seems they do not lead to import dumps. This is understandable because their public section
* is rarely changed (and it might also help that they are called dynamically)

*----------------------------------------------------------------------*
* Documentation of special cases:

* See also Word document: "Konzept Transportprüfungen - Critical Object Checks"

* 18.03.2021 Transport of change to REPS RV07A902 (VOFM condition) --> no import dumps
*            This is correctly not reported by the check logic.
*            However, I had expected import dumps. It seems that function groups (like global classes)
*            are only invalidated by changes to the function modules or if the whole function group is transported.
*            (there are so far only 3 dumps from SAPL... main programs in ZSDUMP_HISTORY)
*----------------------------------------------------------------------*
* Changes:
* 05.11.2019 Edo von Glan
*   Also check enhancement (components), and BAdI implementing classes
* 21.-30.01.2020 Edo von Glan
*   Bugfix/Concept changed: If the changed object is DDIC (not code), we cannot ignore class methods etc.
*   For changed to DDIC objects, all users (not only those that will be re-generated) count.
* 17.02.2020 Edo von Glan
*   Bugfix: Do not report objects with popular objects below warning threshold
*   Bugfix: do not count function groups as well as their frame programs
*   Bugfix: non-popular objects should not be in list of popular objects
*   Bugfix: wrong indentation of buffer results
* 21.02.2020 Edo von Glan
*   Resolve discrepancies with result of old check for D1DK9A1D5D
*   Bugfixes in threshold and output handling
* 26.02.2020 Edo von Glan
*   Bugfix: Frame Program SAPLMEPO should be resolved to FUGR MEPO
* 17.12.2020 Edo von Glan
*   Bugfix: Don't follow BAdI-Interfaces if implementation class is all new
* 09.03.2021 Edo von Glan
*   Add export parameter e_has_errors for get_findings_text, to distinguish between errors and warnings in SolMan
* 19.03.2021 Edo von Glan
*   Bugfix: for FUGR, recursion was to LIMU LIMU instead of correctly LIMU FUNC
* 20.08.2021: Edo von Glan
*   Change to new logger from https://github.com/ABAP-Logger/ABAP-Logger (but with slightly adapted class name)
*   Make logger call dynamic
*----------------------------------------------------------------------*
*
* Poosible improvements
* 21.04.2021: Maybe it would be good to replace adir_key with cts_object_key, because contents from e071 (with longer obj_name) can go into this.
*             On the other hand, this would require a retest. And so far, no problems have occurred.
*
************************************************************************
  ENDMETHOD.


  METHOD display_findings.
    DATA dummy TYPE string.
    CHECK lcl_main=>s_messages IS NOT INITIAL.
    DATA(extnumber) = COND balnrext( WHEN lines( s_it_trkorr ) = 0 THEN |{ s_tr_objects[ 1 ]-pgmid }/{ s_tr_objects[ 1 ]-object }/{ s_tr_objects[ 1 ]-obj_name }|
                                     ELSE                               |{ s_it_trkorr[ 1 ] }| ).
    IF lines( s_it_trkorr ) + lines( s_tr_objects ) > 1.
      extnumber = |{ extnumber } ...|.
    ENDIF.
    IF s_with_analysis = abap_true.
      extnumber = |Test { extnumber }|.
    ENDIF.
    DATA(logger) = get_logger( extnumber ).
    IF logger IS NOT BOUND.
      WRITE /.
      WRITE /.
      ULINE.
      WRITE / 'This is the fallback output with a classical list.'.
      WRITE / 'Try to install a logger class in your system, then adapt the code in'.
      WRITE / 'zcl_s_trkorr_chk_critical_obj to use that logger.'.
      WRITE / 'Recommended logger: https://github.com/ABAP-Logger/ABAP-Logger.'.
      ULINE.
      WRITE /.
      WRITE /.
    ENDIF.

    LOOP AT lcl_main=>s_messages REFERENCE INTO DATA(finding).
      MESSAGE ID     finding->msgid
              TYPE   finding->msgty
              NUMBER finding->msgno
              WITH   finding->msgv1
                     finding->msgv2
                     finding->msgv3
                     finding->msgv4 INTO dummy.
      IF logger IS BOUND.
        CALL METHOD logger->('ADD').
      ELSE.
        WRITE / dummy.
      ENDIF.
    ENDLOOP.
    IF logger IS BOUND.
      CALL METHOD logger->('SAVE').
      CALL METHOD logger->('FULLSCREEN').
    ENDIF.
  ENDMETHOD.


  METHOD get_findings_text.
    e_has_error = abap_false.
    CHECK lcl_main=>s_messages IS NOT INITIAL.
    LOOP AT lcl_main=>s_messages REFERENCE INTO DATA(finding) WHERE msgno <> '022' OR msgid <> 'ZS_DEV_TOOLS_LOCAL'. " remove the message about transport to D1Q
      MESSAGE ID     finding->msgid
              TYPE   finding->msgty
              NUMBER finding->msgno
              WITH   finding->msgv1
                     finding->msgv2
                     finding->msgv3
                     finding->msgv4 INTO DATA(line).
      APPEND line TO r.
      IF 'EAX' CS finding->msgty.
        e_has_error = abap_true.
      ENDIF.
    ENDLOOP.
    DATA(trkorr) = VALUE #( s_it_trkorr[ 1 ] DEFAULT '???' ).
    MESSAGE i035 WITH sy-sysid trkorr INTO line.
    APPEND line TO r.

    DATA(trkorr_lines) = lines( s_it_trkorr ).
    ASSERT ID zlog FIELDS trkorr_lines CONDITION trkorr_lines = 1. " when called from SolMan, there should be exactly one trkorr
  ENDMETHOD.


  METHOD store_findings.
    DATA dummy TYPE string.
    CHECK lcl_main=>s_messages IS NOT INITIAL.
    DATA(logger) = get_logger( i_extnumber ).
    ASSERT logger IS BOUND. " to use this method, install a logger

    LOOP AT lcl_main=>s_messages REFERENCE INTO DATA(finding).
      MESSAGE ID     finding->msgid
              TYPE   finding->msgty
              NUMBER finding->msgno
              WITH   finding->msgv1
                     finding->msgv2
                     finding->msgv3
                     finding->msgv4 INTO dummy.
      CALL METHOD logger->('ADD').
    ENDLOOP.

    CALL METHOD logger->('SAVE').
  ENDMETHOD.

  METHOD get_logger.
* This is the coding if you are sure that you have zcl_x_logger_factory in your system:
*    DATA(logger) = zcl_x_logger_factory=>create_log(
*          object                   = 'ZS_DEV'
*          subobject                = 'TRKORR_CRIT'
*          desc                     = extnumber
*          retention_period         = 90 ).
* This is the dynamic coding if you are not sure:
    CALL METHOD cl_abap_typedescr=>describe_by_name( " classic syntax necessary to allow catching the classic exception
      EXPORTING
        p_name         = '\CLASS=' && con_logger_name
      RECEIVING
        p_descr_ref    = DATA(rtti_generic)
      EXCEPTIONS
        type_not_found = 1 ).
    CHECK sy-subrc = 0.
    DATA(rtti_class) = CAST cl_abap_objectdescr( rtti_generic ).
    DATA(method) = rtti_class->methods[ name = 'CREATE_LOG' ].
    DATA(params) = VALUE abap_parmbind_tab( ( kind = 'E' name = 'OBJECT'    value = NEW bal_s_log-object( 'ZS_DEV' ) )
                                            ( kind = 'E' name = 'SUBOBJECT' value = NEW bal_s_log-subobject( 'TRKORR_CRIT' ) )
                                            ( kind = 'E' name = 'DESC'      value = REF #( i_extnumber ) )
                                            ( kind = 'R' name = 'R_LOG'     value = REF #( r ) ) ).
    IF line_exists( method-parameters[ name = 'RETENTION_PERIOD' ] ).   " additional parameter used by Draeger
      INSERT VALUE #( kind = 'E' name = 'RETENTION_PERIOD'  value = NEW dec4_0( 90 ) ) INTO TABLE params.
    ENDIF.

    CALL METHOD (con_logger_name)=>('CREATE_LOG') PARAMETER-TABLE params.
  ENDMETHOD.

ENDCLASS.
