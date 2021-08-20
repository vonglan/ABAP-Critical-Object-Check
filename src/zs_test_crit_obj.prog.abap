*----------------------------------------------------------------------*
*                         D r a e g e r   I T                          *
*----------------------------------------------------------------------*
* Developer Tools / Check transports for critical objects
*----------------------------------------------------------------------*
* Checks transports whether they contain critical objects
* that might lead to dumps when the transport is imported
* to the production system
*
*----------------------------------------------------------------------*
* If you want to use this in a different company:
* - It is recommended to install a logger class, for example https://github.com/ABAP-Logger/ABAP-Logger.
* - You then need to adapt class zcl_s_trkorr_chk_critical.
* - You might want to call the logic in the transport release BAdI,
*   method METHOD if_ex_cts_request_check~check_before_release, like this:
*    zcl_s_trkorr_chk_critical_obj=>check( i_it_trkorr = value #( ( request ) ) ).
*    zcl_s_trkorr_chk_critical_obj=>display_findings( ).
*   This works only with logger installed
* - Adapt long text for message ZS_DEV_CRIT_OBJ012 according to your company's policy
*----------------------------------------------------------------------*
* Author: Edo von Glan
* Date  : ca. 2018
*----------------------------------------------------------------------*
REPORT zs_test_crit_obj.

TABLES e070.

SELECT-OPTIONS: s_trkorr FOR e070-trkorr.
PARAMETERS: p_analys TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK two WITH FRAME TITLE TEXT-001.
PARAMETERS: p_nr_dep TYPE i DEFAULT 200,
            p_nr_err TYPE i DEFAULT 5, " increased from 2 to 5. EvG 23.03.2021
            p_min    TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK two.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK one WITH FRAME TITLE TEXT-000.
PARAMETERS: p_pgm  TYPE tadir-pgmid DEFAULT 'R3TR',
            p_obj  TYPE tadir-object,
            p_name TYPE tadir-obj_name.
SELECTION-SCREEN END OF BLOCK one.

AT SELECTION-SCREEN ON p_nr_dep.
  IF p_nr_dep IS NOT INITIAL AND p_nr_dep < zcl_s_trkorr_chk_critical_obj=>s_error_threshold_dep_obj.
    MESSAGE e034(zs_dev_crit_obj) WITH zcl_s_trkorr_chk_critical_obj=>s_error_threshold_dep_obj.
  ENDIF.

START-OF-SELECTION.
  IF s_trkorr IS NOT INITIAL.
    SELECT trkorr
      FROM e070
      WHERE trkorr IN @s_trkorr
      INTO TABLE @DATA(it_trkorr).
    CHECK sy-subrc = 0.
    CLEAR p_pgm.
  ENDIF.

  CHECK it_trkorr IS NOT INITIAL OR p_name IS NOT INITIAL.

  DATA(objects) = COND tadir_key_t( WHEN p_name IS INITIAL THEN VALUE #( )
                                    ELSE VALUE #( ( pgmid    = p_pgm
                                                    object   = p_obj
                                                    obj_name = p_name ) ) ).
  zcl_s_trkorr_chk_critical_obj=>check( i_it_trkorr = CONV #( it_trkorr )
                                        i_tr_objects = objects
                                        i_with_analysis = p_analys
                                        i_threshold_nr_dependent_objs = p_nr_dep
                                        i_threshold_nr_errors = p_nr_err
                                        i_threshold_duration_min = p_min ).
  zcl_s_trkorr_chk_critical_obj=>display_findings( ).
