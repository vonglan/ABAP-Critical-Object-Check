CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    TYPES:
      ttp_obj_hash TYPE HASHED TABLE OF adir_key WITH UNIQUE KEY pgmid object obj_name,
      ttp_obj      TYPE STANDARD TABLE OF adir_key WITH DEFAULT KEY,

      BEGIN OF tp_popular_obj,
        object              TYPE adir_key,
        nr_calling_packages TYPE i,
      END OF tp_popular_obj,
      ttp_popular_obj_sorted TYPE SORTED TABLE OF tp_popular_obj WITH UNIQUE KEY object,

      BEGIN OF tp_check_result,
        object                      TYPE adir_key,
        nr_dependent_objects        TYPE i,
        critical_objects            TYPE ttp_obj,
        popular_objects             TYPE ttp_popular_obj_sorted,
        is_cpub_only_format_changed TYPE abap_bool,
      END OF tp_check_result,
      ttp_check_result_hash TYPE HASHED   TABLE OF tp_check_result WITH UNIQUE KEY object,
      ttp_check_result      TYPE STANDARD TABLE OF tp_check_result WITH KEY object.

    CLASS-METHODS:
      check
        IMPORTING i_it_trkorr  TYPE trkorrs OPTIONAL
                  i_tr_objects TYPE ttp_obj OPTIONAL.

    CLASS-DATA: s_messages   TYPE zcl_s_trkorr_chk_critical_obj=>ttp_message READ-ONLY.

  PRIVATE SECTION.
    TYPES: BEGIN OF tp_tr_object,
             tr   TYPE adir_key,
             main TYPE adir_key,
           END OF tp_tr_object,
           ttp_tr_objects TYPE STANDARD TABLE OF tp_tr_object WITH KEY tr.

    CLASS-DATA: s_tr_objects                   TYPE ttp_tr_objects,
                s_dummy                        TYPE string,
                s_messages_tmp                 LIKE s_messages,
                s_messages_tmp_contains_errors TYPE abap_bool.
    CLASS-METHODS:
      add_message
        IMPORTING i_final TYPE abap_bool OPTIONAL,
      finalize_tmp_messages,
      create_messages_from_results
        IMPORTING
          i_results TYPE ttp_check_result.
ENDCLASS.

CLASS lcl_threshold DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: con_warning_threshold_dep_obj TYPE i VALUE 20,  " threshold when a warning/error is displayed
               con_error_threshold_dep_obj   TYPE i VALUE 200,
               con_warning_threshold_pop_obj TYPE i VALUE 2,
               con_error_threshold_pop_obj   TYPE i VALUE 10.
    CLASS-DATA: s_threshold_nr_dependent_objs TYPE i READ-ONLY,
                s_threshold_nr_errors         TYPE i READ-ONLY.
    CLASS-METHODS init
      IMPORTING
        !i_threshold_nr_dependent_objs TYPE i  " timeout for this number of dependent objects for one transported object
        !i_threshold_duration_min      TYPE i  " timeout for this total check runtime
        !i_threshold_nr_errors         TYPE i. " timeout for this total number of errors
    CLASS-METHODS is_timeout_reached
      RETURNING VALUE(r) TYPE abap_bool.
    CLASS-METHODS is_nr_errors_reached
      RETURNING VALUE(r) TYPE abap_bool.
    CLASS-METHODS get_any_threshold_message
      RETURNING VALUE(r) TYPE string.
    CLASS-METHODS notify_about_dependent_objects
      IMPORTING
        i_nr_dependent_objects TYPE i.
    CLASS-METHODS notify_about_critical_object
      IMPORTING i_object TYPE adir_key.
    CLASS-METHODS notify_about_popular_object
      IMPORTING i_object TYPE adir_key.
    CLASS-METHODS is_message_available
      RETURNING
        VALUE(r) TYPE abap_bool.
  PRIVATE SECTION.
    CLASS-DATA: s_threshold_timestamp    TYPE timestamp,
                s_nr_errors              TYPE i,
                s_threshold_duration_min TYPE i,
                s_critical_objects       TYPE STANDARD TABLE OF adir_key,
                s_popular_objects        TYPE STANDARD TABLE OF adir_key.
ENDCLASS.


CLASS lcl_critical_objects DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS is_critical
      IMPORTING i_object TYPE adir_key
      RETURNING VALUE(r) TYPE abap_bool.
    CLASS-METHODS is_user_exit
      IMPORTING
        i_object TYPE adir_key
      RETURNING
        VALUE(r) TYPE abap_bool.
    CLASS-METHODS check_user_exit
      IMPORTING
        i_object        TYPE adir_key
      RETURNING
        VALUE(r_result) TYPE lcl_main=>tp_check_result.
  PRIVATE SECTION.
    CLASS-DATA: s_critical_objects TYPE lcl_main=>ttp_obj_hash.
ENDCLASS.

CLASS lcl_dependent_objects DEFINITION.
  PUBLIC SECTION.
    CONSTANTS con_sql_pattern_sure_glob_meth TYPE progname VALUE '_____________________________=CM%'.
    CLASS-METHODS:
      init,
      check
        IMPORTING i_object               TYPE adir_key
                  i_flg_is_trkorr_object TYPE abap_bool OPTIONAL
        RETURNING VALUE(r_result)        TYPE lcl_main=>tp_check_result,
      add_txt
        IMPORTING i_txt TYPE string,
      add_to_result
        IMPORTING
          i_increase TYPE lcl_main=>tp_check_result
        CHANGING
          c_result   TYPE lcl_main=>tp_check_result.
    CLASS-DATA: s_depth                        TYPE i,
                s_recursion_stack              TYPE STANDARD TABLE OF adir_key,
                s_shallow_mode_threshld_reachd TYPE abap_bool,
                s_eval_generation_only_not_usg TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_tabl_view_ttyp_object
        IMPORTING i_object        TYPE adir_key
        RETURNING VALUE(r_result) TYPE lcl_main=>tp_check_result,
      check_dtel_object
        IMPORTING i_object        TYPE adir_key
        RETURNING VALUE(r_result) TYPE lcl_main=>tp_check_result,
      check_fugr_object
        IMPORTING i_object        TYPE adir_key
        RETURNING VALUE(r_result) TYPE lcl_main=>tp_check_result,
      check_enho_object
        IMPORTING i_object        TYPE adir_key
        RETURNING VALUE(r_result) TYPE lcl_main=>tp_check_result,
      get_objects_for_recursion
        IMPORTING i_include        TYPE progname
        RETURNING VALUE(r_objects) TYPE lcl_main=>ttp_obj,
      check_thresholds,
      check_any_object
        IMPORTING i_object TYPE adir_key
        CHANGING  c_result TYPE lcl_main=>tp_check_result,
      write_summary
        IMPORTING i_result TYPE lcl_main=>tp_check_result,
      check_buffers
        IMPORTING i_object    TYPE adir_key
        EXPORTING e_result    TYPE lcl_main=>tp_check_result
                  e_do_return TYPE abap_bool,
      is_endless_loop
        IMPORTING i_object TYPE adir_key
        RETURNING VALUE(r) TYPE abap_bool,
      init_for_new_object
        IMPORTING i_object TYPE adir_key.
    CLASS-DATA: s_result_buffer_tr_obj   TYPE lcl_main=>ttp_check_result_hash,
                s_result_buffer_all      TYPE lcl_main=>ttp_check_result_hash,
                s_perf_info_buffer_uses  TYPE int8,  " number of successful buffer accesses
                s_perf_info_buffer_saves TYPE int8,  " number of saved check() calls through buffer
                s_trkorr_object          TYPE adir_key,
                s_trkorr_object_dep_obj  TYPE i,
                s_dont_buffer_results    TYPE abap_bool.
ENDCLASS.

CLASS lcl_popular_objects DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS check
      IMPORTING i_object        TYPE adir_key
      RETURNING VALUE(r_result) TYPE lcl_main=>tp_check_result.
  PRIVATE SECTION.
    TYPES: BEGIN OF tp_caller,
             called         TYPE wbcrossgt-name,
             caller         TYPE wbcrossgt-include,
             caller_name    TYPE progname,
             caller_package TYPE devclass,
           END OF tp_caller,
           ttp_caller TYPE STANDARD TABLE OF tp_caller WITH DEFAULT KEY.
    CLASS-METHODS get_callers
      IMPORTING
                i_object    TYPE adir_key
      RETURNING VALUE(r_it) TYPE ttp_caller.
    CLASS-METHODS get_called_package
      IMPORTING
        i_object TYPE adir_key
      RETURNING
        VALUE(r) TYPE devclass.
    CLASS-METHODS get_intfs_of_badi_impl_class
      IMPORTING
        i_object TYPE adir_key
      RETURNING
        VALUE(r) TYPE seo_class_names.
ENDCLASS.

CLASS lcl_util DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_main_object
        IMPORTING i_sub         TYPE adir_key
        RETURNING VALUE(r_main) TYPE adir_key,
      is_cpub_only_format_changed
        IMPORTING i_classname TYPE seoclsname
        RETURNING VALUE(r)    TYPE abap_bool,
      get_tokens_for_class
        IMPORTING i_classname        TYPE seoclsname
                  i_version          TYPE versno
        RETURNING VALUE(r_token_tab) TYPE stringtab,
      determine_object_from_include
        IMPORTING i_include       TYPE string
        RETURNING VALUE(r_object) TYPE string,
      get_package_from_frameprogram
        IMPORTING i_name   TYPE tadir-obj_name
        RETURNING VALUE(r) TYPE devclass,
      get_includers
        IMPORTING
          i_include_name TYPE adir_key-obj_name
        EXPORTING
          e_mainprograms TYPE prognames
          e_trkey        TYPE trkey,
      get_function_from_include
        IMPORTING
          i_include TYPE progname
        RETURNING
          VALUE(r)  TYPE tfdir-funcname,
      is_same_area
        IMPORTING
          i_pkg1   TYPE devclass
          i_pkg2   TYPE devclass
        RETURNING
          VALUE(r) TYPE abap_bool,
      is_class_but_not_public_sectn
        IMPORTING
          i_include TYPE adir_key-obj_name
        RETURNING
          VALUE(r)  TYPE abap_bool,
      is_include_program
        IMPORTING
          i_include TYPE progname
        RETURNING
          VALUE(r)  TYPE abap_bool,
      get_r3tr_object_from_limu_obj
        IMPORTING
                  !i_limu_object       TYPE cts_object_key
        RETURNING VALUE(r_r3tr_object) TYPE adir_key.

  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD check.
    DATA: results    TYPE lcl_main=>ttp_check_result,
          tr_objects TYPE ttp_obj.
    CLEAR: s_messages,
           s_tr_objects.
    IF i_it_trkorr IS NOT INITIAL.
      SELECT pgmid, object, obj_name
        FROM e071
        FOR ALL ENTRIES IN @i_it_trkorr
        WHERE trkorr = @i_it_trkorr-table_line
          AND objfunc <> 'D'
        INTO TABLE @tr_objects.
      SORT tr_objects BY table_line.
      DELETE ADJACENT DUPLICATES FROM tr_objects COMPARING table_line.
    ENDIF.
    APPEND LINES OF i_tr_objects TO tr_objects.

    DELETE tr_objects WHERE pgmid = 'R3TR' AND ( object = 'TEXT'     " texts
                                              OR object = 'SMIM'     " MIME obejcts
                                              OR object = 'NOTE'     " notes
                                              OR object = 'CINS'     " correction instructions
                                              OR object = 'TABU' ).  " table content is not relevant (e.g. LANG TABU T006A), and no main object can be determined

    LOOP AT tr_objects REFERENCE INTO DATA(tr_object).
      IF tr_object->pgmid = 'R3TR'.
        s_tr_objects = VALUE #( BASE s_tr_objects
                                ( tr   = tr_object->*
                                  main = tr_object->* ) ).
      ELSEIF tr_object->pgmid = 'LIMU' OR tr_object->pgmid = 'LANG'.  " translation changes lead to re-activation!
        " Mail from Joerg Wrage 11.10.2016: "RV45A wurde heute aufgrund des Transports D1DK999643 neu aktiviert.
        "                                    Dort wurde eine Übersetzung für Englisch auf DataElement VBELN_VL eingespielt."
        IF tr_object->object = 'FUNC'. " special handling to allow checks on subobject in this case
          s_tr_objects = VALUE #( BASE s_tr_objects
                                  ( tr   = tr_object->*
                                    main = tr_object->* ) ).
        ELSE.
          s_tr_objects = VALUE #( BASE s_tr_objects
                                  ( tr   = tr_object->*
                                    main = lcl_util=>get_r3tr_object_from_limu_obj( CONV #( tr_object->* ) ) ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE s_tr_objects WHERE main-object = 'CLAS' AND ( tr-object <> 'CPUB' AND tr-object <> 'CLAS' ). " for classes, changes to internals are safe. Only PUBLIC SECTION changes lead to import dumps.

    lcl_critical_objects=>init( ).
    lcl_dependent_objects=>init( ).

    LOOP AT s_tr_objects REFERENCE INTO DATA(obj) WHERE main IS NOT INITIAL
                                                  GROUP BY ( object   = obj->main-object
                                                             obj_name = obj->main-obj_name ).
      DATA(loop_index) = sy-tabix.
      DATA(result) = lcl_dependent_objects=>check( i_object               = obj->main
                                                   i_flg_is_trkorr_object = abap_true ).
      lcl_threshold=>notify_about_dependent_objects( result-nr_dependent_objects ).
      INSERT result INTO TABLE results.
      cl_progress_indicator=>progress_indicate( i_text      = replace( val = TEXT-000 sub = '##' with = |{ obj->main-object } { obj->main-obj_name }| )
                                                i_processed = loop_index
                                                i_total     = lines( s_tr_objects )
                                                i_output_immediately = abap_true ).

      DATA(stop_reason) = lcl_threshold=>get_any_threshold_message( ).
      IF stop_reason IS NOT INITIAL.
        IF zcl_s_trkorr_chk_critical_obj=>s_with_analysis = abap_true.
          WRITE /  |...|.
          WRITE /  |...|.
          WRITE /  |...|.
          WRITE / |---- Stopping analysis completely - { stop_reason }|.
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_progress_indicator=>progress_indicate( i_text               = TEXT-002
                                              i_output_immediately = abap_true ).

    create_messages_from_results( results ).

  ENDMETHOD.

  METHOD add_message.
    DATA msg LIKE LINE OF s_messages.
    msg = VALUE #( msgid    = sy-msgid
                    msgno    = sy-msgno
                    msgty    = sy-msgty
                    msgv1    = sy-msgv1
                    msgv2    = sy-msgv2
                    msgv3    = sy-msgv3
                    msgv4    = sy-msgv4 ).
    IF i_final = abap_true.
      APPEND msg TO s_messages.
    ELSE.
      APPEND msg TO s_messages_tmp.
      IF sy-msgty CA 'AXEW'.
        s_messages_tmp_contains_errors = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD finalize_tmp_messages.
    IF s_messages_tmp_contains_errors = abap_true.
      APPEND LINES OF s_messages_tmp TO s_messages.
    ENDIF.
    CLEAR s_messages_tmp.
    s_messages_tmp_contains_errors = abap_false.
  ENDMETHOD.

  METHOD create_messages_from_results.
    DATA(results_sorted) = i_results.
    SORT results_sorted BY nr_dependent_objects DESCENDING.

    MESSAGE i012 INTO s_dummy.
    add_message( i_final = abap_true ).

    LOOP AT results_sorted REFERENCE INTO DATA(wa) WHERE critical_objects IS NOT INITIAL
                                                      OR popular_objects IS NOT INITIAL
                                                      OR nr_dependent_objects > lcl_threshold=>con_warning_threshold_dep_obj.
      MESSAGE i011 WITH wa->object-pgmid wa->object-object wa->object-obj_name INTO s_dummy.
      add_message( ).
      LOOP AT wa->critical_objects REFERENCE INTO DATA(crit_obj).
        MESSAGE e025 WITH crit_obj->object crit_obj->obj_name INTO s_dummy.  " Führt sehr wahrscheinl. zu Abbrüchen, da in krit. Objekt &1 &2 verwendet
        add_message( ).
      ENDLOOP.
      LOOP AT wa->popular_objects REFERENCE INTO DATA(pop_obj).
        IF pop_obj->nr_calling_packages > lcl_threshold=>con_error_threshold_pop_obj.
          MESSAGE e029 WITH |{ pop_obj->object-object } { pop_obj->object-obj_name }|
                                                                   pop_obj->nr_calling_packages INTO s_dummy.  " Führt sehr wahrscheinl. zu Abbrüchen, da in &1 anderen Paketen verwendet
          add_message( ).
        ELSEIF pop_obj->nr_calling_packages > lcl_threshold=>con_warning_threshold_pop_obj.
          MESSAGE w028 WITH |{ pop_obj->object-object } { pop_obj->object-obj_name }|
                                                                   pop_obj->nr_calling_packages INTO s_dummy.  " Kann zu Abbrüchen führen, da in &1 anderen Paketen verwendet
          add_message( ).
        ENDIF.
      ENDLOOP.
      IF lcl_threshold=>s_threshold_nr_dependent_objs IS NOT INITIAL AND wa->nr_dependent_objects >= lcl_threshold=>s_threshold_nr_dependent_objs.
        MESSAGE i031 INTO s_dummy.                                             " ... (i.e. further dependent objects have not been investigated, but might be critical or popular
        add_message( ).
        MESSAGE e030 WITH lcl_threshold=>s_threshold_nr_dependent_objs INTO s_dummy.            " Führt sehr wahrscheinl. zu Abbrüchen, da in mehr als &1 Entw.objekten verwend.
        add_message( ).
      ELSEIF wa->nr_dependent_objects > lcl_threshold=>con_error_threshold_dep_obj.
        MESSAGE e016 WITH wa->nr_dependent_objects INTO s_dummy.               " Führt sehr wahrscheinl. zu Abbrüchen, da in ca. &1 Entw.objekten verwend.
        add_message( ).
      ELSEIF wa->nr_dependent_objects > lcl_threshold=>con_warning_threshold_dep_obj.
        MESSAGE w015 WITH wa->nr_dependent_objects INTO s_dummy.               " Kann zu Abbrüchen führen, da in ca. &1 anderen Entw.objekten verwendet
        add_message( ).
      ENDIF.
      IF     wa->object-object = 'CLAS'
         AND     line_exists( s_tr_objects[ tr-object   = 'CPUB'
                                        tr-obj_name = wa->object-obj_name ] )
         AND NOT line_exists( s_tr_objects[ tr-object   = 'CLAS'
                                        tr-obj_name = wa->object-obj_name ] )
         AND lcl_util=>is_cpub_only_format_changed( i_classname = EXACT #( wa->object-obj_name ) ).
        MESSAGE e021 INTO s_dummy.
        add_message( ).
      ENDIF.
      finalize_tmp_messages( ).
    ENDLOOP.

    IF lines( s_messages ) = 1. " if there is only the introduction message
      CLEAR s_messages.
      RETURN.
    ENDIF.

    IF lcl_threshold=>is_message_available( ).
      DATA(msg_save) = sy.
      MESSAGE i031 INTO s_dummy. " ...
      add_message( i_final = abap_true ).
      sy-msgid = msg_save-msgid.
      sy-msgno = msg_save-msgno.
      sy-msgty = msg_save-msgty.
      sy-msgv1 = msg_save-msgv1. " we know we have only one argument
      add_message( i_final = abap_true ).
    ENDIF.

    MESSAGE i022 INTO s_dummy. " - Note: these messages do not block the transport to D1Q
    add_message( i_final = abap_true ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dependent_objects IMPLEMENTATION.
  METHOD check.
* possible values for i_object:
* - all R3TR (main) object types
* - FUNC = function module
* - PROG = include program of unknown type (might be frame program or whatever)
    ASSERT i_object-pgmid IS NOT INITIAL.

    " new transport object --> init
    IF i_flg_is_trkorr_object = abap_true.
      init_for_new_object( i_object ).
    ENDIF.

    " check whether we should stop after n dependent objects, n total errors, total runtime n minutes
    check_thresholds( ).

    " try to get result from buffer (first, the buffer for the current transport object, then the overall buffer)
    check_buffers( EXPORTING i_object = i_object
                   IMPORTING e_result = r_result
                             e_do_return = DATA(do_return) ).
    CHECK do_return = abap_false.

    " avoid endless loops because of mutual or cyclic dependencies
    CHECK NOT is_endless_loop( i_object ).

    IF s_shallow_mode_threshld_reachd = abap_false.
      " check whether object is configured as critical
      IF lcl_critical_objects=>is_critical( i_object ).
        INSERT i_object INTO TABLE r_result-critical_objects.
        DATA(critical_msg) = | (CRITICAL OBJECT)|.
      ENDIF.
      " actual processing (find users/includers)
      add_txt( |Analyzing users of { i_object-object } { i_object-obj_name }{ critical_msg }:| ).
      check_any_object( EXPORTING i_object = i_object
                        CHANGING  c_result = r_result ).
    ENDIF.

    " check for transport object finished --> write summary to classical list
    IF s_depth = 1.
      write_summary( r_result ).
    ENDIF.

    " housekeeping
    ADD 1 TO r_result-nr_dependent_objects.
    ADD 1 TO s_trkorr_object_dep_obj.
    IF s_trkorr_object_dep_obj MOD 50 = 0 AND s_trkorr_object_dep_obj > 0. " count 0 can occur in initial steps of tree traversal
      DATA(progress_text) = replace( val = TEXT-004 sub = '&1' with = |{ s_trkorr_object-object } { s_trkorr_object-obj_name }| ).
      progress_text = replace( val = progress_text sub = '&2' with = |{ s_trkorr_object_dep_obj }| ).
      cl_progress_indicator=>progress_indicate( i_text = progress_text
                                                i_output_immediately = abap_true ).
    ENDIF.
    DELETE s_recursion_stack INDEX s_depth.
    s_depth = s_depth - 1.
    IF s_dont_buffer_results = abap_false.
      INSERT r_result INTO TABLE s_result_buffer_tr_obj.
      INSERT r_result INTO TABLE s_result_buffer_all.
    ENDIF.
  ENDMETHOD.

  METHOD check_tabl_view_ttyp_object.
    DATA includes    TYPE STANDARD TABLE OF adir_key.
    ASSERT s_eval_generation_only_not_usg = abap_false. " if we check a DDIC object, the origin (trkorr object)
    " must have been a DDIC object as well. In this case, we always need to follow the complete usage chain

    " part 1: Usage in tables (as APPENDs or INCLUDEs) or by views
    IF i_object-object = 'TABL'.
      SELECT SINGLE 'R3TR' AS pgmid, 'TABL' AS object, sqltab AS obj_name
        FROM dd02l
        WHERE tabname = @i_object-obj_name
          AND tabclass = 'APPEND'
        INTO @DATA(appended_table).
      IF sy-subrc = 0.
        add_to_result( EXPORTING i_increase = check( CORRESPONDING #( appended_table ) )
                       CHANGING  c_result   = r_result ).
      ENDIF.

      SELECT 'R3TR' AS pgmid, 'TABL' AS object, tabname AS obj_name
        FROM dd03l
        WHERE precfield = @i_object-obj_name
          AND adminfield < '1'
          AND fieldname LIKE '.INCL%'
          AND fieldname <> '.INCLU--AP'
        ORDER BY tabname
          INTO TABLE @DATA(including_tables).
      LOOP AT including_tables REFERENCE INTO DATA(including_table).
        CHECK s_shallow_mode_threshld_reachd = abap_false.
        add_to_result( EXPORTING i_increase = check( CORRESPONDING #( including_table->* ) )
                       CHANGING  c_result   = r_result ).
      ENDLOOP.

      SELECT 'R3TR' AS pgmid, 'VIEW' AS object, tabname AS obj_name
        FROM dd02l
        WHERE sqltab = @i_object-obj_name
          AND tabclass = 'VIEW'
          ORDER BY tabname
        INTO TABLE @DATA(views).
      LOOP AT views REFERENCE INTO DATA(view).
        CHECK s_shallow_mode_threshld_reachd = abap_false.
        add_to_result( EXPORTING i_increase = check( CORRESPONDING #( view->* ) )
                       CHANGING  c_result   = r_result ).
      ENDLOOP.
    ENDIF.

    " part 2: Usage in search helps
    IF i_object-object = 'TABL'.
      SELECT shlpname
      FROM dd30l
      WHERE selmethod = @i_object-obj_name
        AND selmtype IN ( 'T', 'X', 'V' )   " DB-Tables without or with Text Tables, Views (not clear why also "V", but TABL ZSD_M_DEBIA is an example)
      INTO TABLE @DATA(search_helps).
    ENDIF.
    IF i_object-object = 'VIEW'.
      SELECT shlpname
      FROM dd30l
      WHERE selmethod = @i_object-obj_name
        AND selmtype IN ( 'V', 'H' )  " DB-Views and Help-Views
      INTO TABLE @search_helps.
    ENDIF.
    LOOP AT search_helps REFERENCE INTO DATA(search_help).
      CHECK s_shallow_mode_threshld_reachd = abap_false.
      add_to_result( EXPORTING i_increase = check( VALUE #( pgmid    = 'R3TR'
                                                            object   = 'SHLP'
                                                            obj_name = EXACT #( search_help->shlpname ) ) )
                     CHANGING  c_result   = r_result ).
    ENDLOOP.

    " part 3: Usage in programs (and classes and functions)
    SELECT 'R3TR' AS pgmid, 'PROG' AS object, include AS obj_name
      FROM wbcrossgt
      WHERE name  = @i_object-obj_name
        AND otype = 'TY'
        ORDER BY include
      INTO CORRESPONDING FIELDS OF TABLE @includes.
    LOOP AT includes REFERENCE INTO DATA(include).
      CHECK s_shallow_mode_threshld_reachd = abap_false.
      add_to_result( EXPORTING i_increase = check( CORRESPONDING #( include->* ) )
                     CHANGING  c_result   = r_result ).
    ENDLOOP.


  ENDMETHOD.

  METHOD check_fugr_object.
    DATA functions TYPE STANDARD TABLE OF rs38l_incl.
    DATA(function_group) = EXACT rs38l_area( i_object-obj_name ).
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = function_group
      TABLES
        functab                 = functions       " Alle Funktionsbausteine der Funktionsgruppe
      EXCEPTIONS
        function_pool_not_found = 1             " Die Funktionsgruppe wurde nicht gefunden
        OTHERS                  = 2.
    ASSERT ID zlog FIELDS i_object-obj_name CONDITION sy-subrc = 0. " could not determine functions for function group - maybe deleted
    LOOP AT functions REFERENCE INTO DATA(func).
      CHECK s_shallow_mode_threshld_reachd = abap_false.
      add_to_result( EXPORTING i_increase = check( VALUE #( pgmid    = 'LIMU'
                                                            object   = 'FUNC' " bugfix 19.03.2021 - was 'LIMU'
                                                            obj_name = func->funcname ) )
                     CHANGING  c_result   = r_result ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_dtel_object.
    DATA includes    TYPE STANDARD TABLE OF adir_key.
    " part 1: Usage in DDIC tables and structures
    SELECT 'R3TR' AS pgmid, 'TABL' AS object, tabname AS obj_name
      FROM dd03l
      WHERE rollname = @i_object-obj_name
      ORDER BY tabname
      INTO TABLE @DATA(tabls).
    LOOP AT tabls REFERENCE INTO DATA(tabl).
      CHECK s_shallow_mode_threshld_reachd = abap_false.
      add_to_result( EXPORTING i_increase = check( CORRESPONDING #( tabl->* ) )
                     CHANGING  c_result   = r_result ).
    ENDLOOP.

    " part 2: Usage in include programs (reports, classes, functions, ...)
    ASSERT s_eval_generation_only_not_usg = abap_false. " if we check a DDIC object, the origin (trkorr object)
    " must have been a DDIC object as well. In this case, we always need to follow the complete usage chain
    SELECT 'R3TR' AS pgmid, 'PROG' AS object, include AS obj_name
      FROM wbcrossgt
      WHERE name  = @i_object-obj_name
        AND otype = 'TY'
      INTO CORRESPONDING FIELDS OF TABLE @includes.
    LOOP AT includes REFERENCE INTO DATA(include).
      CHECK s_shallow_mode_threshld_reachd = abap_false.
      add_to_result( EXPORTING i_increase = check( CORRESPONDING #( include->* ) )
                     CHANGING  c_result   = r_result ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_to_result.
    ADD i_increase-nr_dependent_objects TO c_result-nr_dependent_objects.
    LOOP AT i_increase-critical_objects REFERENCE INTO DATA(crit_obj).
      COLLECT crit_obj->* INTO c_result-critical_objects.
    ENDLOOP.
    LOOP AT i_increase-popular_objects REFERENCE INTO DATA(pop_obj).
      COLLECT pop_obj->* INTO c_result-popular_objects.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_txt.
    IF zcl_s_trkorr_chk_critical_obj=>s_with_analysis = abap_true.
      WRITE / |{ '' WIDTH = s_depth * 2  }{ i_txt }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_objects_for_recursion.
    DATA(function) = lcl_util=>get_function_from_include( i_include ).
    IF function IS NOT INITIAL.
      INSERT VALUE #( pgmid    = 'LIMU'
                      object   = 'FUNC'
                      obj_name = function ) INTO TABLE r_objects.
    ELSE.
      lcl_util=>get_includers( EXPORTING i_include_name = i_include
                               IMPORTING e_mainprograms = DATA(mainprograms)
                                         e_trkey        = DATA(trkey) ).

      IF trkey IS NOT INITIAL.  " objects that are directly in TADIR, e.g. MV45AFZZ
        IF trkey-obj_type = 'PROG' AND trkey-obj_name = i_include.
          ASSERT  ID zlog FIELDS i_include CONDITION mainprograms IS NOT INITIAL. " unexpected case - TADIR entry for Include program but no mainprogram
        ELSE.
          INSERT VALUE #( pgmid    = 'R3TR'
                          object   = trkey-obj_type
                          obj_name = trkey-obj_name ) INTO TABLE r_objects.
        ENDIF.
      ENDIF.

      LOOP AT mainprograms REFERENCE INTO DATA(mainprog).
        IF trkey-obj_type <> 'FUGR' OR mainprog->* NP |SAPL{ trkey-obj_name }|. " avoid duplicate FUGR + frameprogram
          DATA trkey_mainprog TYPE trkey.
          CALL FUNCTION 'RS_GET_TRKEY_FOR_TRDIR_NAME'
            EXPORTING
              p_trdir_name                 = mainprog->*
            IMPORTING
              es_trkey                     = trkey_mainprog
            EXCEPTIONS
              no_wb_object_type_found      = 1                 " Keinen Workbench Object Type gefunden
              no_mapping_found             = 2                 " Es wurde keine Zuordnung zu einem WB-Objekttyp gefunden
              no_unique_mapping            = 3                 " Die Zuordnung zwischen Transport-Objekttyp und WB-Objekttyp
              other_errors_wb_type         = 4                 " Andere Fehler bei WB-Type Errmittlung
              not_decideable_trkey         = 5                 " TRKEY nicht ermittelbar
              no_valid_tadir_entry_found   = 6                 " Kein gültiger TADIR Eintrag zum Objekt gefunden
              no_devclass_found_for_object = 7                 " Zum Objekt wurde keine Entwicklungsklasse gefunden
              delimiter_error              = 8                 " Delimiter_error
              other_errors_name_split      = 9                 " Other Errors during Name Split Call
              error_determining_mainprogs  = 10                " Fehler beim ermitteln des Hauptprograms
              generated_coding             = 11                " Generiertes Coding wird nicht unterstützt
              OTHERS                       = 12.
          ASSERT ID zlog FIELDS mainprog->* sy-subrc CONDITION sy-subrc = 0.
          INSERT VALUE #( pgmid    = 'R3TR'
                          object   = trkey_mainprog-obj_type
                          obj_name = trkey_mainprog-obj_name ) INTO TABLE r_objects.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD check_thresholds.
    IF lcl_threshold=>s_threshold_nr_dependent_objs IS NOT INITIAL AND s_trkorr_object_dep_obj > lcl_threshold=>s_threshold_nr_dependent_objs AND s_shallow_mode_threshld_reachd = abap_false.  " only once
      DATA(stop_reason) = |{ lcl_threshold=>s_threshold_nr_dependent_objs } dependent objects identified|.
      DATA(stop_scope) = |for { s_trkorr_object-object } { s_trkorr_object-obj_name } |.
    ELSE.
      stop_reason = lcl_threshold=>get_any_threshold_message( ).
      IF stop_reason IS NOT INITIAL.
        stop_scope = |completely |.
        add_txt(  |...| ).
        add_txt(  |...| ).
        add_txt(  |...| ).
      ENDIF.
    ENDIF.
    IF stop_reason IS NOT INITIAL.
      add_txt( |---- Stopping analysis { stop_scope }- { stop_reason }| ).
      s_shallow_mode_threshld_reachd = abap_true.
      s_dont_buffer_results = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_any_object.
    CASE i_object-object.
      WHEN 'DOMA'. " usage in DTELS
        SELECT 'R3TR' AS pgmid, 'DTEL' AS object, rollname AS obj_name
          FROM dd04l
          WHERE domname = @i_object-obj_name
          ORDER BY rollname
          INTO TABLE @DATA(dtels)
          .
        LOOP AT dtels REFERENCE INTO DATA(dtel).
          CHECK s_shallow_mode_threshld_reachd = abap_false.
          add_to_result( EXPORTING i_increase = check( CORRESPONDING #( dtel->* ) )
                         CHANGING  c_result   = c_result ).
        ENDLOOP.
      WHEN 'DTEL'.
        add_to_result( EXPORTING i_increase = check_dtel_object( i_object )
                       CHANGING  c_result   = c_result ).
      WHEN 'TABL' OR 'VIEW' OR 'TTYP'.
        add_to_result( EXPORTING i_increase = check_tabl_view_ttyp_object( i_object )
                       CHANGING  c_result   = c_result ).
      WHEN 'FUNC'.
        add_to_result( EXPORTING i_increase = lcl_popular_objects=>check( i_object )
                       CHANGING  c_result   = c_result ).
      WHEN 'FUGR' OR 'FUGS' OR 'FUGX'.
        IF s_trkorr_object = i_object. " recurse from function group to function modules ONLY IF this is the transport object (because lower down in recursion, the direction is opposite)
          add_to_result( EXPORTING i_increase = check_fugr_object( i_object )
                         CHANGING  c_result   = c_result ).
        ENDIF.
      WHEN 'PROG'.
        IF lcl_critical_objects=>is_user_exit( i_object ).
          add_to_result( EXPORTING i_increase = lcl_critical_objects=>check_user_exit( i_object )
                         CHANGING  c_result   = c_result ).
          RETURN.
        ENDIF.
        IF lcl_util=>is_include_program( i_object-obj_name ).
          SUBTRACT 1 FROM c_result-nr_dependent_objects. " i.e. do not count include programs on their own (but increase indentation etc.)
          SUBTRACT 1 FROM s_trkorr_object_dep_obj.
          DATA(includer_objects) = get_objects_for_recursion( i_object-obj_name ).
          LOOP AT includer_objects REFERENCE INTO DATA(incl_obj).
            CHECK s_shallow_mode_threshld_reachd = abap_false.
            add_to_result( EXPORTING i_increase = check( incl_obj->* )
                           CHANGING  c_result   = c_result ).
          ENDLOOP.
        ELSE.
          " no deeper recursion necessary, if not include program
        ENDIF.
      WHEN 'CLAS' OR 'INTF'.
        add_to_result( EXPORTING i_increase = lcl_popular_objects=>check( i_object )
                       CHANGING  c_result   = c_result ).
      WHEN 'ENHO'.
        add_to_result( EXPORTING i_increase = check_enho_object( i_object )
                       CHANGING  c_result   = c_result ).
    ENDCASE.
  ENDMETHOD.

  METHOD write_summary.
    IF i_result-critical_objects IS NOT INITIAL.
      DATA(more_text) = |{ lines( i_result-critical_objects ) } critical objects |.
    ENDIF.
    IF i_result-popular_objects IS NOT INITIAL.
      more_text = |{ more_text } { lines( i_result-popular_objects ) } popular objects|.
    ENDIF.
    add_txt( |---- Result for { i_result-object-object } { i_result-object-obj_name }: { i_result-nr_dependent_objects } dependent objects { more_text }| ).
    IF sy-uname = 'VONGLANR' AND 1 = 2.
      add_txt( |---- Performance Info: { s_perf_info_buffer_saves } check() calls saved with { s_perf_info_buffer_uses } buffer reads. | &
               |Buffer size for this tr obj: { lines( s_result_buffer_tr_obj ) }, overall: { lines( s_result_buffer_all ) }| ).
    ENDIF.
  ENDMETHOD.

  METHOD check_buffers.
    e_result = VALUE #( s_result_buffer_tr_obj[ object = i_object ]
                        DEFAULT VALUE #( ) ).
    IF e_result IS NOT INITIAL.
      ADD 1 TO s_perf_info_buffer_uses.
      " increased indentation, because s_depth has not yet been increased
      add_txt( |  Analyzing { i_object-object } { i_object-obj_name } - in buffer for this transport object - not counted again| ).
      s_dont_buffer_results = abap_true. " because the buffer was used and duplicates were ignored
      e_do_return = abap_true.
      RETURN.
    ENDIF.
    e_result = VALUE #( s_result_buffer_all[ object = i_object ]
                        DEFAULT VALUE #( ) ).
    IF e_result IS NOT INITIAL.
      ADD 1 TO s_perf_info_buffer_uses.
      ADD e_result-nr_dependent_objects TO s_perf_info_buffer_saves.
      add_txt( |  Analyzing { i_object-object } { i_object-obj_name } - in overall buffer - adding { e_result-nr_dependent_objects } dependent objects| ).
      e_do_return = abap_true.
      RETURN.
    ENDIF.
    e_result = VALUE #( object = i_object ).
  ENDMETHOD.

  METHOD is_endless_loop.
    IF line_exists( s_recursion_stack[ pgmid    = i_object-pgmid
                                       object   = i_object-object
                                       obj_name = i_object-obj_name ] ).
      add_txt( |NOT Analyzing users of { i_object-object } { i_object-obj_name } - avoid endless loop| ).
      s_dont_buffer_results = abap_true. " reason: when there is a cyclic dependency for transport object A at dependent object O, there might not be one for transport object B at dependent object O
      r = abap_true.
      RETURN.
    ENDIF.
    INSERT i_object INTO TABLE s_recursion_stack.
    s_depth = s_depth + 1.
    ASSERT lines(  s_recursion_stack ) = s_depth.
  ENDMETHOD.

  METHOD init_for_new_object.
    s_trkorr_object = i_object.
    ASSERT s_depth IS INITIAL AND s_recursion_stack IS INITIAL.
    s_eval_generation_only_not_usg = COND #( WHEN 'PROG REPS FUGR FUNC CLAS INTF CLSD CPUB CPRI CPRO METH CINC' CS i_object-object THEN abap_true ELSE abap_false ).
    CLEAR: s_perf_info_buffer_saves, s_perf_info_buffer_uses,
           s_trkorr_object_dep_obj,
           s_shallow_mode_threshld_reachd,
           s_dont_buffer_results,
           s_result_buffer_tr_obj.
  ENDMETHOD.

  METHOD init.
    CLEAR: s_result_buffer_tr_obj,
           s_result_buffer_all,
           s_perf_info_buffer_uses,
           s_perf_info_buffer_saves,
           s_trkorr_object,
           s_trkorr_object_dep_obj,
           s_dont_buffer_results.
  ENDMETHOD.

  METHOD check_enho_object.
    SELECT SINGLE @abap_true FROM enhheader
      WHERE enhname = @i_object-obj_name
        AND version = 'A'
        AND enhtooltype <> 'BADI_IMPL'  " BAdIs are not interesting on their own, only the class - tested in lcl_popular_objects
        INTO @DATA(is_non_badi_enhancement).
    IF is_non_badi_enhancement = abap_true.
      SELECT pgmid, obj_type AS object, obj_name
        FROM enhobj
        WHERE enhname = @i_object-obj_name
        AND pgmid = 'R3TR'
        ORDER BY object, obj_name
        INTO TABLE @DATA(enhanced_objects).
      LOOP AT enhanced_objects REFERENCE INTO DATA(object).
        CHECK s_shallow_mode_threshld_reachd = abap_false.
        add_to_result( EXPORTING i_increase = check( CORRESPONDING #( object->* ) )
                       CHANGING  c_result   = r_result ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.
  METHOD get_includers.
    DATA: nr_mainprogs TYPE i,
          main_program TYPE progname,
          trkey        TYPE trkey.
    CLEAR: e_trkey, e_mainprograms.
    CALL FUNCTION 'RS_GET_TRKEY_FOR_TRDIR_NAME'
      EXPORTING
        p_trdir_name                 = i_include_name
      IMPORTING
        es_trkey                     = trkey
        p_main_program               = main_program
        p_nr_of_mainprogs            = nr_mainprogs
      EXCEPTIONS
        no_wb_object_type_found      = 1                 " Keinen Workbench Object Type gefunden
        no_mapping_found             = 2                 " Es wurde keine Zuordnung zu einem WB-Objekttyp gefunden
        no_unique_mapping            = 3                 " Die Zuordnung zwischen Transport-Objekttyp und WB-Objekttyp
        other_errors_wb_type         = 4                 " Andere Fehler bei WB-Type Errmittlung
        not_decideable_trkey         = 5                 " TRKEY nicht ermittelbar - EvG: this is always the case for Business Objects, e.g. /NFM/RBUS2030
        no_valid_tadir_entry_found   = 6                 " Kein gültiger TADIR Eintrag zum Objekt gefunden
        no_devclass_found_for_object = 0                 " Zum Objekt wurde keine Entwicklungsklasse gefunden - EvG: Does not matter
        delimiter_error              = 8                 " Delimiter_error
        other_errors_name_split      = 9                 " Other Errors during Name Split Call
        error_determining_mainprogs  = 10                " Fehler beim ermitteln des Hauptprograms
        generated_coding             = 11                " Generiertes Coding wird nicht unterstützt
        OTHERS                       = 12.
    CASE sy-subrc.
      WHEN 0. " ok
        e_trkey = trkey.
        IF nr_mainprogs <= 1 AND main_program IS NOT INITIAL.
          e_mainprograms = VALUE #( ( main_program ) ).
        ELSE.
          CALL FUNCTION 'RS_GET_MAINPROGRAMS'
            EXPORTING
              name         = i_include_name
            TABLES
              mainprograms = e_mainprograms
            EXCEPTIONS
              OTHERS       = 0.
          IF e_mainprograms IS INITIAL.
            SELECT master
              FROM wbcrossi
              WHERE otype = 'IC'
                AND name = @i_include_name
              INTO TABLE @e_mainprograms.
            IF sy-subrc <> 0.
              lcl_dependent_objects=>add_txt( |No includers found for { i_include_name }| ).
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 5.
        lcl_dependent_objects=>add_txt( |ADMIN Warning: TRKEY cannot be determined for { i_include_name }| ).
      WHEN 11.
        LOG-POINT ID zlog FIELDS i_include_name. " RS_GET_TRKEY_FOR_TRDIR_NAME sy-subrc=11 - generated coding
        lcl_dependent_objects=>add_txt( |ADMIN Warning: { i_include_name } reported as generated coding - further analysis not possible| ).
      WHEN OTHERS.
        LOG-POINT ID zlog FIELDS sy-subrc i_include_name. " RS_GET_TRKEY_FOR_TRDIR_NAME sy-subrc
        lcl_dependent_objects=>add_txt( |ADMIN Warning: sy-subrc { sy-subrc } from RS_GET_TRKEY_FOR_TRDIR_NAME for { i_include_name }| ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_main_object.
    r_main = CORRESPONDING #( get_r3tr_object_from_limu_obj( CONV #( i_sub ) ) ).
    IF r_main IS INITIAL.
      IF i_sub-object = 'FUNC'.
        SELECT SINGLE @abap_true
          FROM tfdir
         WHERE funcname = @i_sub-obj_name
         INTO @DATA(dummy).
        ASSERT ID zlog FIELDS i_sub-object i_sub-obj_name CONDITION sy-subrc <> 0. " Cannot determine R3TR object from LIMU object. Is FUNC but not deleted
      ELSE.
        ASSERT ID zlog FIELDS i_sub-object i_sub-obj_name CONDITION sy-subrc = 0.  " Cannot determine R3TR object from LIMU object
      ENDIF.
    ENDIF.
    r_main-pgmid = 'R3TR'.
  ENDMETHOD.

  METHOD get_r3tr_object_from_limu_obj.
    " The standard SAP function GET_R3TR_OBJECT_FROM_LIMU_OBJ is buggy (for various types, e.g. DTED, DOMD, MESS, it returns the DEVC).
    " Therefore we use TR_CHECK_TYPE and encapsulate it here
    DATA(limu_e071) = CORRESPONDING e071( i_limu_object ).
    ASSERT ID zlog FIELDS sy-cprog sy-tcode CONDITION limu_e071-pgmid IS INITIAL OR limu_e071-pgmid = 'LIMU'.
    limu_e071-pgmid = 'LIMU'.
    DATA r3tr_tadir TYPE tadir.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071  = limu_e071
      IMPORTING
        we_tadir = r3tr_tadir.
    r_r3tr_object = CORRESPONDING #( r3tr_tadir ).

    IF r_r3tr_object-obj_name IS INITIAL.
      ASSERT ID zlog FIELDS limu_e071 CONDITION limu_e071-object = 'FUNC'.
      CLEAR r_r3tr_object.
    ENDIF.
  ENDMETHOD.

  METHOD is_cpub_only_format_changed.
    DATA: vrsn_tab     TYPE STANDARD TABLE OF vrsn,
          vrsd_tab     TYPE STANDARD TABLE OF vrsd_old,
          classname_34 TYPE char34.
    r = abap_false.
    classname_34 = i_classname.
    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY'
      EXPORTING
        objname      = classname_34
        objtype      = 'CPUB'
      TABLES
        lversno_list = vrsn_tab
        version_list = vrsd_tab
      EXCEPTIONS
        OTHERS       = 1.
    ASSERT ID zlog FIELDS classname_34 CONDITION sy-subrc = 0. " info for class not found
    CHECK sy-subrc = 0 AND lines( vrsd_tab ) > 1.
    DATA(version_trkorrs) = VALUE trkorrs( FOR <version> IN vrsd_tab ( <version>-korrnum ) ). " need to convert format (not possible in SQL)
    CHECK version_trkorrs IS NOT INITIAL. " for ATC ;-)
    SELECT trkorr FROM e070
      FOR ALL ENTRIES IN @version_trkorrs
        WHERE trkorr = @version_trkorrs-table_line
          AND trkorr <> ''
          AND trfunction <> 'T'
        INTO TABLE @DATA(non_toc_trkorrs).
    LOOP AT vrsd_tab REFERENCE INTO DATA(version) FROM 2.
      IF line_exists( non_toc_trkorrs[ table_line = version->korrnum ] ).
        r = xsdbool( get_tokens_for_class( i_classname = i_classname
                                           i_version   = vrsd_tab[ 1 ]-versno )
                   = get_tokens_for_class( i_classname = i_classname
                                           i_version   = version->versno ) ).
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_tokens_for_class.
    DATA: repos_tab  TYPE STANDARD TABLE OF abaptxt255,
          trdir_tab  TYPE STANDARD TABLE OF trdir,
          tokens     TYPE STANDARD TABLE OF stokes,
          statements TYPE STANDARD TABLE OF sstmnt ##needed,
          classname  TYPE versobjnam.
    classname = i_classname.
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name                  = classname
        object_type                  = 'CPUB'
        versno                       = i_version
        iv_no_release_transformation = 'X'
      TABLES
        repos_tab                    = repos_tab
        trdir_tab                    = trdir_tab
      EXCEPTIONS
        OTHERS                       = 0.
    SCAN ABAP-SOURCE repos_tab
      TOKENS      INTO tokens
      STATEMENTS  INTO statements.
    r_token_tab = VALUE #( FOR token IN tokens ( token-str ) ).
  ENDMETHOD.

  METHOD determine_object_from_include.
    r_object = i_include.
    REPLACE REGEX |[\\\\=].*|       IN r_object WITH ||.    " remove everything after = or \
    REPLACE REGEX |(ZCL_.\{26\}).*| IN r_object WITH |$1|.  " keep a maximum of 30 characters for ZCL
    REPLACE REGEX |(ZIF_.\{26\}).*| IN r_object WITH |$1|.  " keep a maximum of 30 characters for ZIF
  ENDMETHOD.

  METHOD get_package_from_frameprogram.
    SELECT SINGLE devclass FROM tadir
      WHERE pgmid = 'R3TR'
        AND obj_name = @i_name
        INTO @r.
    CHECK sy-subrc <> 0.

    IF i_name CP '*SAPL*'.  " function groups
      DATA(name) = i_name.
      REPLACE REGEX |SAPL| IN name WITH ||.
      SELECT SINGLE devclass FROM tadir
        WHERE pgmid = 'R3TR'
          AND obj_name = @name
        INTO @r.
    ENDIF.
    CHECK sy-subrc <> 0.

    IF i_name CP '*SAPDB*'.  " logical data base
      name = i_name.
      REPLACE REGEX |SAPDB| IN name WITH ||.
      SELECT SINGLE devclass FROM tadir
        WHERE pgmid = 'R3TR'
          AND object = 'LDBA'
          AND obj_name = @name
        INTO @r.
    ENDIF.
    CHECK sy-subrc <> 0.

    name = i_name. " classes
    REPLACE REGEX |[\\\\=].*|      IN name WITH ||.
    name = name(30).
    SELECT SINGLE devclass FROM tadir
    WHERE pgmid = 'R3TR'
      AND obj_name = @name
        INTO @r.
    CHECK sy-subrc <> 0.

    IF i_name CP '/1BCWDY/*'. " web dynpro
      SELECT SINGLE component_name                      "#EC CI_NOFIRST
        FROM wdy_wb_sourcemap
        WHERE inclname = @i_name
        INTO @name.
      SELECT SINGLE devclass FROM tadir
        WHERE pgmid = 'R3TR'
          AND obj_name = @name
        INTO @r.
    ENDIF.

    " BAdIs in Enhancement Spots
    IF strlen( i_name ) <= 30.
      SELECT SINGLE enhspotname
        FROM badi_spot
        WHERE badi_name = @i_name
        INTO @DATA(spot).
      IF sy-subrc = 0.
        SELECT SINGLE devclass FROM tadir
          WHERE pgmid = 'R3TR'
            AND object = 'ENHS'
            AND obj_name = @spot
            INTO @r.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_function_from_include.
    DATA trkey TYPE trkey.
    CHECK matches( val = i_include regex = |.*U\\d\\d| ).
    CALL FUNCTION 'RS_GET_TRKEY_FOR_TRDIR_NAME'
      EXPORTING
        p_trdir_name                 = i_include
      IMPORTING
        es_trkey                     = trkey
      EXCEPTIONS
        no_wb_object_type_found      = 1                 " Keinen Workbench Object Type gefunden
        no_mapping_found             = 2                 " Es wurde keine Zuordnung zu einem WB-Objekttyp gefunden
        no_unique_mapping            = 3                 " Die Zuordnung zwischen Transport-Objekttyp und WB-Objekttyp
        other_errors_wb_type         = 4                 " Andere Fehler bei WB-Type Errmittlung
        not_decideable_trkey         = 5                 " TRKEY nicht ermittelbar
        no_valid_tadir_entry_found   = 6                 " Kein gültiger TADIR Eintrag zum Objekt gefunden
        no_devclass_found_for_object = 7                 " Zum Objekt wurde keine Entwicklungsklasse gefunden
        delimiter_error              = 8                 " Delimiter_error
        other_errors_name_split      = 9                 " Other Errors during Name Split Call
        error_determining_mainprogs  = 10                " Fehler beim ermitteln des Hauptprograms
        generated_coding             = 11                " Generiertes Coding wird nicht unterstützt
        OTHERS                       = 12.
    CHECK sy-subrc = 0.
    IF trkey-sub_type = 'FUNC'.
      r = trkey-sub_name.
    ENDIF.
  ENDMETHOD.

  METHOD is_same_area.
    DATA: name1      TYPE char30,
          name2      TYPE char30,
          namespace1 TYPE trnspace-namespace,
          namespace2 TYPE trnspace-namespace,
          stem       TYPE trobj_name.
    name1 = i_pkg1.
    name2 = i_pkg2.
    IF name1(1) <> '/'.
      CHECK name1(4) = name2(4).
    ELSE.
      CHECK name2(1) = '/'.
      DATA(fullname) = CONV trobj_name( name1 ).
      CALL FUNCTION 'TRINT_SPLIT_OBJECT'
        EXPORTING
          iv_obj_name    = fullname
        IMPORTING
          ev_prefix_name = namespace1
          ev_stem        = stem
        EXCEPTIONS
          invalid_prefix = 1
          OTHERS         = 2.
      ASSERT ID zlog FIELDS name1 CONDITION sy-subrc = 0. " could not split name into namespace + stem
      name1 = stem.
      fullname = CONV trobj_name( name2 ).
      CALL FUNCTION 'TRINT_SPLIT_OBJECT'
        EXPORTING
          iv_obj_name    = fullname
        IMPORTING
          ev_prefix_name = namespace2
          ev_stem        = stem
        EXCEPTIONS
          invalid_prefix = 1
          OTHERS         = 2.
      ASSERT ID zlog FIELDS name1 CONDITION sy-subrc = 0. " could not split name into namespace + stem
      name2 = stem.
      CHECK namespace1 = namespace2.
      CHECK name1(2) = name2(2).
    ENDIF.
    r = abap_true.
  ENDMETHOD.

  METHOD is_class_but_not_public_sectn.
    CHECK strlen( i_include ) >= 31 AND i_include+30(1) = 'C' AND i_include+31(1) <> 'U'.
    IF i_include+29(1) = '='.
      r = abap_true.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = 'CLAS'
        AND obj_name = @i_include(30)
      INTO @DATA(dummy).
    IF sy-subrc = 0.
      r = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_include_program.
    SELECT SINGLE @abap_true
      FROM reposrc
      WHERE progname = @i_include
        AND r3state = 'A'
        AND subc = 'I'
        INTO @DATA(dummy).
    r = xsdbool(  sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_critical_objects IMPLEMENTATION.
  METHOD init.
    DATA tadir_entries TYPE lcl_main=>ttp_obj.
    CHECK s_critical_objects IS INITIAL.
    SELECT *
      FROM zstr_crit_objs
      INTO TABLE @DATA(critical_object_patterns).
    LOOP AT critical_object_patterns REFERENCE INTO DATA(pattern).
      DATA(name_pattern) = pattern->obj_name.
      TRANSLATE name_pattern USING |*%+_|.  " ABAP wildcards --> SQL wildcards
      SELECT pgmid, object, obj_name
        FROM tadir
        WHERE pgmid  = @pattern->pgmid
          AND object = @pattern->object
          AND obj_name LIKE @name_pattern
        INTO CORRESPONDING FIELDS OF TABLE @tadir_entries.
      IF sy-subrc <> 0.
        LOG-POINT ID zlog FIELDS pattern->object pattern->obj_name. " not TADIR objects found for customizing
        lcl_dependent_objects=>add_txt( |ADMIN Warning: No TADIR objects found for { pattern->object } { pattern->obj_name }| ).
      ENDIF.
      LOOP AT tadir_entries REFERENCE INTO DATA(tadir_entry).
        COLLECT tadir_entry->* INTO s_critical_objects.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_critical.
    IF line_exists( s_critical_objects[ pgmid    = i_object-pgmid
                                        object   = i_object-object
                                        obj_name = i_object-obj_name ] ).
      r = abap_true.
      lcl_threshold=>notify_about_critical_object( i_object ).
    ENDIF.
  ENDMETHOD.

  METHOD is_user_exit.
    IF i_object-obj_name CP 'ZX*'.
      ASSERT i_object-object = 'PROG'.
      SELECT SINGLE @abap_true
        FROM wbcrossi
        WHERE name = @i_object-obj_name
          AND master LIKE 'SAPLX%'
        INTO @r.
    ELSEIF i_object-object CP 'FUG*' AND i_object-obj_name CP 'X*'.
      r = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_user_exit.
    DATA crit_obj LIKE i_object.
    IF i_object-obj_name CP 'ZX*'.
      SELECT SINGLE include
        FROM wbcrossi
        WHERE name = @i_object-obj_name
          AND master LIKE 'SAPLX%'
        INTO @DATA(include).
      CHECK sy-subrc = 0.
      IF include CP 'LX*U++'.
        DATA(func) = lcl_util=>get_function_from_include( include ).
        crit_obj = VALUE #( object = 'FUNC' obj_name = func ).
      ELSE.
        crit_obj = VALUE #( object = 'FUGR' obj_name = include+4 ).
      ENDIF.
    ELSEIF i_object-object CP 'FUG*' AND i_object-obj_name CP 'X*'.
      crit_obj = i_object.
    ENDIF.
    IF crit_obj IS NOT INITIAL.
      lcl_dependent_objects=>add_txt( |  { crit_obj-object } { crit_obj-obj_name } is CRITICAL OBJECT (Customer Exit)| ).
      r_result = VALUE #( critical_objects = VALUE #( ( crit_obj ) ) ).
      lcl_threshold=>notify_about_critical_object( crit_obj ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_popular_objects IMPLEMENTATION.
  METHOD check.
    r_result = VALUE #( object          = i_object
                        popular_objects = VALUE #( ( object = i_object ) ) ).
    DATA(caller_reference) = get_callers( i_object ).
    DATA(called_obj) = i_object-obj_name.
    DATA(called_package) = get_called_package( i_object ).
    lcl_dependent_objects=>add_txt( |  .. in pkg { called_package }| ).

    LOOP AT caller_reference REFERENCE INTO DATA(caller).
      caller->caller_name = lcl_util=>determine_object_from_include( CONV string( caller->caller ) ).
      IF called_obj = caller->caller_name.
        DELETE caller_reference USING KEY loop_key.
        CONTINUE.
      ENDIF.

      caller->caller_package = lcl_util=>get_package_from_frameprogram( caller->caller_name ).
      IF caller->caller_package IS INITIAL.
        SELECT SINGLE master
          FROM wbcrossi
         WHERE name = @caller->caller_name
          INTO @DATA(caller_frame_program).
        IF sy-subrc = 0.
          caller->caller_package = lcl_util=>get_package_from_frameprogram( caller_frame_program ).
        ENDIF.
      ENDIF.
      ASSERT ID zlog FIELDS i_object-object i_object-obj_name caller->caller_name CONDITION caller->caller_package IS NOT INITIAL. " cannot determine package (not from include, not from frame program)
    ENDLOOP.

    " Count calls from other packages
    IF lcl_dependent_objects=>s_eval_generation_only_not_usg = abap_true. " popular objects are only relevant in the Generation Chain; in the Usage Chain only the number of users count
      LOOP AT caller_reference REFERENCE INTO caller GROUP BY caller->caller_package.
        IF called_package <> caller->caller_package.
          IF lcl_util=>is_same_area( i_pkg1 = called_package
                                     i_pkg2 = caller->caller_package ).
            lcl_dependent_objects=>add_txt( |  .. used by pkg { caller->caller_package } - seems to be same area - not counted (e.g. { caller->caller_name })| ).
          ELSE.
            lcl_dependent_objects=>add_txt( |  .. used by pkg { caller->caller_package } (e.g. { caller->caller_name })| ).
            r_result-popular_objects[ 1 ]-nr_calling_packages = r_result-popular_objects[ 1 ]-nr_calling_packages + 1.
          ENDIF.
        ELSE.
          lcl_dependent_objects=>add_txt( |  .. used by own pkg { caller->caller_package } (e.g. { caller->caller_name })| ).
        ENDIF.
      ENDLOOP.
      IF r_result-popular_objects[ 1 ]-nr_calling_packages >= lcl_threshold=>con_error_threshold_pop_obj.
        lcl_dependent_objects=>add_txt( |  POPULAR object (used by { r_result-popular_objects[ 1 ]-nr_calling_packages } other packages)| ).
        lcl_threshold=>notify_about_popular_object( i_object ).
      ELSEIF r_result-popular_objects[ 1 ]-nr_calling_packages >= lcl_threshold=>con_warning_threshold_pop_obj.
        lcl_dependent_objects=>add_txt( |  Popular object (used by { r_result-popular_objects[ 1 ]-nr_calling_packages } other packages)| ).
      ELSE.
        CLEAR r_result-popular_objects. " forget it, if it is not worth a WARNING
      ENDIF.
    ELSE.
      CLEAR r_result-popular_objects. " forget it, if popularity has not been evaluated
    ENDIF.

    " Critical object check for function group
    IF i_object-object = 'FUNC'.
      DATA(function_group) = lcl_util=>get_main_object( i_object ).
      IF lcl_critical_objects=>is_critical( function_group ).
        INSERT function_group INTO TABLE r_result-critical_objects.
        lcl_dependent_objects=>add_txt( |Function group { function_group-obj_name } is CRITICAL OBJECT| ).
      ENDIF.
    ENDIF.
    " Recursion to direct callers
    LOOP AT caller_reference REFERENCE INTO DATA(include).
      CHECK lcl_dependent_objects=>s_shallow_mode_threshld_reachd = abap_false.
      lcl_dependent_objects=>add_to_result( EXPORTING i_increase = lcl_dependent_objects=>check( VALUE #( pgmid    = 'R3TR'
                                                                                                          object   = 'PROG'
                                                                                                          obj_name = include->caller ) )
                                            CHANGING  c_result   = r_result ).
    ENDLOOP.
    " For BAdi implementation classes, recursion to implemented interfaces (i.e. indirect callers)
    DATA(badi_interfaces) = get_intfs_of_badi_impl_class( i_object ).
    IF badi_interfaces IS NOT INITIAL.
      " 17.12.2020 - also a problem if the class is new, because (at least in the case 17.12.2020)
      " the PUBLIC SECTION of a class with the name of the enhancement spot (example: WORKORDER_UPDATE==============CU)
      " is re-generated internally.
      " If this ever changes, the following code would be useful.
*      SELECT SINGLE @abap_true
*        FROM vrsd
*        WHERE objtype = 'CPUB'
*          AND objname = @i_object-obj_name
*"          AND datum <> @sy-datum     would not be correct if there is a new object with a correction later the same day
*          INTO @DATA(class_not_new).
*      IF class_not_new = abap_true.
      lcl_dependent_objects=>add_txt( |Special case: For BAdi implementation classes, recursion to implemented interfaces (i.e. indirect callers)| ).
    ENDIF.
    LOOP AT badi_interfaces REFERENCE INTO DATA(badi_interface).
      CHECK lcl_dependent_objects=>s_shallow_mode_threshld_reachd = abap_false.
      lcl_dependent_objects=>add_to_result( EXPORTING i_increase = lcl_dependent_objects=>check( VALUE #( pgmid    = 'R3TR'
                                                                                                          object   = 'INTF'
                                                                                                          obj_name = badi_interface->* ) )
                                            CHANGING  c_result   = r_result ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_callers.
    IF i_object-object = 'CLAS' OR i_object-object = 'INTF'.
      DATA(oo_pattern) = i_object-obj_name && '%'.
      IF lcl_dependent_objects=>s_eval_generation_only_not_usg = abap_true. " evaluate generation-chain only
        SELECT name AS called,
               include AS caller
           FROM wbcrossgt
          WHERE name LIKE @oo_pattern
             AND include NOT LIKE @lcl_dependent_objects=>con_sql_pattern_sure_glob_meth  " performance - exclude global class methods
           INTO CORRESPONDING FIELDS OF TABLE @r_it
           UP TO 5000 ROWS.   " increases the performance, because a better Oracle DB access path is chosen by the system
      ELSE.                                                                 " evaluate full usage-chain
        SELECT name AS called,
               include AS caller
           FROM wbcrossgt
          WHERE name LIKE @oo_pattern
           INTO CORRESPONDING FIELDS OF TABLE @r_it
           UP TO 5000 ROWS.   " increases the performance, because a better Oracle DB access path is chosen by the system
      ENDIF.
      ASSERT ID zlog FIELDS oo_pattern CONDITION sy-dbcnt < 5000.  " unexpected quantity of callers (more than 5000)
      LOOP AT r_it REFERENCE INTO DATA(wa) .                                          " \\: needs to be escaped twice (once for string template, once for regex) to mean just the character '\'
        IF NOT matches( val = wa->called regex = |{ i_object-obj_name }\\\\?.*| )  " discard wrong "called" records (longer names, found due to LIKE)
           OR  matches( val = wa->caller regex = |{ i_object-obj_name }\\\\?.*| ). " discard self-usage1
          DELETE r_it USING KEY loop_key.
        ENDIF.
      ENDLOOP.
    ELSE.
      ASSERT i_object-object = 'FUNC'.
      IF lcl_dependent_objects=>s_eval_generation_only_not_usg = abap_true.
        SELECT name AS called,
               include AS caller
          FROM cross
         WHERE type = 'F'
           AND name = @i_object-obj_name
           AND include NOT LIKE @lcl_dependent_objects=>con_sql_pattern_sure_glob_meth
          INTO CORRESPONDING FIELDS OF TABLE @r_it.
      ELSE.
        SELECT name AS called,
               include AS caller
          FROM cross
         WHERE type = 'F'
           AND name = @i_object-obj_name
          INTO CORRESPONDING FIELDS OF TABLE @r_it.
      ENDIF.
    ENDIF.
    SORT r_it BY caller.
    DELETE ADJACENT DUPLICATES FROM r_it COMPARING caller.
    IF lcl_dependent_objects=>s_eval_generation_only_not_usg = abap_true.
      LOOP AT r_it REFERENCE INTO wa.
        IF lcl_util=>is_class_but_not_public_sectn( wa->caller ).
          DELETE r_it USING KEY loop_key.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_called_package.
    IF r IS INITIAL.
      IF i_object-object = 'FUNC'.
        SELECT SINGLE pname
          FROM tfdir
         WHERE funcname = @i_object-obj_name
         INTO @DATA(called_frame_program).
        ASSERT ID zlog FIELDS i_object-obj_name CONDITION sy-subrc = 0. " cannot determine frame program for function module
      ELSE.
        called_frame_program = i_object-obj_name.
      ENDIF.
      r = lcl_util=>get_package_from_frameprogram( called_frame_program ).
    ENDIF.
  ENDMETHOD.

  METHOD get_intfs_of_badi_impl_class.
    TYPES: BEGIN OF ltp,
             enhname  TYPE enhobj-enhname,
             obj_type TYPE enhobj-obj_type,
             obj_name TYPE enhobj-obj_name,
           END OF ltp,
           lttp TYPE STANDARD TABLE OF ltp.
    STATICS: badi_enhanced_objects TYPE lttp.
    CHECK i_object-object = 'CLAS'.
    IF badi_enhanced_objects IS INITIAL.
      SELECT enhname, obj_type, obj_name
        FROM enhobj
        WHERE enhname LIKE 'Z%'
          AND elemusage = 'USEO'
        ORDER BY enhname, obj_type, obj_name
        INTO CORRESPONDING FIELDS OF TABLE @badi_enhanced_objects.
    ENDIF.
    DATA(badi_name) = VALUE enhobj-enhname( badi_enhanced_objects[ obj_type = 'CLAS'
                                                                   obj_name = i_object-obj_name ]  DEFAULT '' ).
    IF badi_name IS NOT INITIAL.
      r = VALUE #( FOR intf IN badi_enhanced_objects WHERE ( obj_type = 'INTF' AND enhname = badi_name ) ( CONV #( intf-obj_name ) ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_threshold IMPLEMENTATION.
  METHOD init.
    s_threshold_nr_dependent_objs = i_threshold_nr_dependent_objs.
    s_threshold_nr_errors         = i_threshold_nr_errors.
    s_threshold_duration_min      = i_threshold_duration_min.
    IF i_threshold_duration_min IS NOT INITIAL.
      GET TIME STAMP FIELD DATA(now).
      TRY.
          s_threshold_timestamp = cl_abap_tstmp=>add( tstmp = now
                                                      secs  = i_threshold_duration_min * 60 ).
        CATCH cx_parameter_invalid_range cx_parameter_invalid_type.
          ASSERT ID zlog FIELDS i_threshold_duration_min now CONDITION 1 = 0.
      ENDTRY.
    ELSE.
      s_threshold_timestamp = '99991231235959'.
    ENDIF.
  ENDMETHOD.

  METHOD is_timeout_reached.
    GET TIME STAMP FIELD DATA(now).
    r = xsdbool( now >= s_threshold_timestamp ).
  ENDMETHOD.

  METHOD is_nr_errors_reached.
    r = xsdbool( s_threshold_nr_errors IS NOT INITIAL
                 AND s_nr_errors >= s_threshold_nr_errors ).
  ENDMETHOD.

  METHOD notify_about_dependent_objects.
    IF i_nr_dependent_objects > con_error_threshold_dep_obj.
      s_nr_errors = s_nr_errors + 1.
    ENDIF.
  ENDMETHOD.

  METHOD notify_about_popular_object.
    CHECK NOT line_exists( s_popular_objects[ pgmid    = i_object-pgmid
                                              object   = i_object-object
                                              obj_name = i_object-obj_name ] ).
    APPEND i_object TO s_popular_objects.
    s_nr_errors = s_nr_errors + 1.
  ENDMETHOD.

  METHOD notify_about_critical_object.
    CHECK NOT line_exists( s_critical_objects[ pgmid    = i_object-pgmid
                                               object   = i_object-object
                                               obj_name = i_object-obj_name ] ).
    APPEND i_object TO s_critical_objects.
    s_nr_errors = s_nr_errors + 1.
  ENDMETHOD.

  METHOD is_message_available.
    IF is_timeout_reached( ).
      MESSAGE i032 WITH s_threshold_duration_min INTO DATA(dummy).
      r = abap_true.
    ENDIF.
    IF is_nr_errors_reached( ).
      MESSAGE i033 WITH s_threshold_nr_errors INTO dummy.
      r = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_any_threshold_message.
    IF lcl_threshold=>is_timeout_reached( ).
      r = |timeout (total runtime reached)|.
    ELSEIF lcl_threshold=>is_nr_errors_reached( ).
      r = |{ lcl_threshold=>s_threshold_nr_errors } errors found|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
