*&---------------------------------------------------------------------*
*& Report z_suim_ca_custom
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_suim_ca_custom_report.

TYPES  : BEGIN OF __ty_ca,
           auth_id   TYPE uscraut-auth_id,
           text      TYPE uscrauidt-text,
           bname     TYPE ususerall-bname,
           name_text TYPE ususerall-name_text,
           class     TYPE ususerall-class,
           gltgv     TYPE ususerall-gltgv,
           gltgb     TYPE ususerall-gltgb,
           accnt     TYPE ususerall-accnt,
           ustyp     TYPE ususerall-ustyp,
         END OF __ty_ca ,

         BEGIN OF __ty_salv_1,
           sysname   TYPE c LENGTH 10,
           systemid  TYPE sy-sysid,
           auth_id   TYPE uscraut-auth_id,
           text      TYPE uscrauidt-text,
           bname     TYPE ususerall-bname,
           name_text TYPE ususerall-name_text,
           class     TYPE ususerall-class,
           gltgv     TYPE ususerall-gltgv,
           gltgb     TYPE ususerall-gltgb,
           accnt     TYPE ususerall-accnt,
           ustyp     TYPE ususerall-ustyp,
           erdat     TYPE xuerdat,
           trdat     TYPE xuldate,
           ltime     TYPE xultime,
           uflag TYPE xuflag,
         END OF __ty_salv_1.

FIELD-SYMBOLS  : <lt_data> TYPE ANY TABLE,
                 <lt_tab>  TYPE any.

DATA :lo_data        TYPE REF TO data,
      lo_display     TYPE REF TO cl_salv_display_settings,
      lo_salv_table  TYPE REF TO cl_salv_table,
      lt_ca          TYPE   STANDARD TABLE OF __ty_ca WITH HEADER LINE,
      lt_ca_custom   TYPE STANDARD TABLE OF zca_cust_tab,
      lt_salv_1      TYPE   STANDARD TABLE OF __ty_salv_1 WITH HEADER LINE,
      lt_swfeature   TYPE STANDARD TABLE OF swfeature,
      lt_usr02       TYPE STANDARD TABLE OF usr02,
      lv_systype(10) TYPE c,
      lv_tsl         TYPE timestampl,
      wa_ca          TYPE __ty_ca,
      wa_ca_custom   TYPE zca_cust_tab,
      wa_salv_1      TYPE __ty_salv_1,
      wa_swfeature   TYPE swfeature,
      wa_usr02       TYPE usr02.

DATA:  lv_cust_table_name TYPE tabname VALUE 'ZSUIMCA_CUST_TAB'.

SELECT  *  FROM swfeature INTO TABLE lt_swfeature.
SORT lt_swfeature BY mod_date.
READ TABLE lt_swfeature INTO  wa_swfeature INDEX 1.
lv_systype = wa_swfeature-name.
CLEAR: lt_swfeature.

cl_salv_bs_runtime_info=>set(
 EXPORTING
   display  = abap_false
   metadata = abap_false
   data     = abap_true
).

CALL FUNCTION 'LIST_FREE_MEMORY'.

SUBMIT rsusr008_009_new
WITH comb = ''
WITH auth = 'X'
WITH authvar = 'ZARDAGH_ACE_REPORTS'
WITH d_analys = 'X'
EXPORTING LIST TO MEMORY
AND RETURN.

TRY.
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lo_data ).
    ASSIGN lo_data->* TO <lt_data>.
  CATCH cx_salv_bs_sc_runtime_info.
    MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
ENDTRY.

cl_salv_bs_runtime_info=>clear_all( ).

LOOP AT <lt_data> ASSIGNING  <lt_tab>.
  MOVE-CORRESPONDING  <lt_tab> TO lt_ca.
  APPEND lt_ca.
ENDLOOP.


REFRESH <lt_data>.
FREE <lt_data>.
CLEAR lo_data.

cl_salv_bs_runtime_info=>set(
 EXPORTING
   display  = abap_false
   metadata = abap_false
   data     = abap_true
).

CALL FUNCTION 'LIST_FREE_MEMORY'.

SUBMIT rsusr200
WITH today = 'X'
WITH valid = 'X'
WITH notvalid = 'X'
WITH locks = 'X'
WITH faillog = 'X'
WITH succlog = 'X'
WITH unused = 'X'
WITH diaguser = 'X'
WITH commuser = 'X'
WITH sysuser = 'X'
WITH servuser = 'X'
WITH refuser = 'X'
WITH defpass = 'X'
WITH initpass = 'X'
WITH nopass = 'X'
EXPORTING LIST TO MEMORY
AND RETURN.

TRY.
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lo_data ).
    ASSIGN lo_data->* TO <lt_data>.
  CATCH cx_salv_bs_sc_runtime_info.
    MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
ENDTRY.

cl_salv_bs_runtime_info=>clear_all( ).


CALL FUNCTION 'LIST_FREE_MEMORY'.

REFRESH <lt_data>.
FREE <lt_data>.
CLEAR lo_data.

SELECT * FROM USR02 INTO table lt_usr02.

IF lt_ca[] IS NOT INITIAL.
  LOOP AT  lt_ca INTO wa_ca.

    wa_salv_1-sysname = lv_systype.
    wa_salv_1-systemid = sy-sysid.
    wa_salv_1-auth_id = wa_ca-auth_id.
    wa_salv_1-text = wa_ca-text.
    wa_salv_1-bname = wa_ca-bname.
    wa_salv_1-name_text = wa_ca-name_text.
    wa_salv_1-class = wa_ca-class.
    wa_salv_1-gltgv = wa_ca-gltgv.
    wa_salv_1-gltgb = wa_ca-gltgb.
    wa_salv_1-accnt = wa_ca-accnt.
    wa_salv_1-ustyp = wa_ca-ustyp.

    CLEAR: wa_usr02.
    READ TABLE lt_usr02 INTO wa_usr02 WITH KEY bname = wa_ca-bname mandt = sy-mandt.
    wa_salv_1-erdat = wa_usr02-erdat.
    wa_salv_1-trdat = wa_usr02-trdat.
    wa_salv_1-ltime = wa_usr02-ltime.
    wa_salv_1-uflag = wa_usr02-uflag.

    APPEND wa_salv_1 TO lt_salv_1.

  ENDLOOP.
ENDIF.

SORT lt_salv_1 BY auth_id bname ASCENDING.

CALL FUNCTION 'ENQUEUE_E_TABLE'
  EXPORTING
    mode_rstable   = 'E'
    tabname        = lv_cust_table_name
  EXCEPTIONS
    foreign_lock   = 1
    system_failure = 2
    OTHERS         = 3.

DELETE FROM zca_cust_tab.
GET TIME STAMP FIELD lv_tsl.
LOOP AT lt_salv_1 INTO wa_salv_1.
  MOVE-CORRESPONDING wa_salv_1 TO wa_ca_custom.
  wa_ca_custom-client = sy-mandt.
  wa_ca_custom-timestamp = lv_tsl.
  APPEND wa_ca_custom TO lt_ca_custom.
ENDLOOP.
MODIFY zca_cust_tab FROM TABLE lt_ca_custom.

CALL FUNCTION 'DEQUEUE_E_TABLE'
  EXPORTING
    mode_rstable = 'E'
    tabname      = lv_cust_table_name.


TRY.
    cl_salv_table=>factory( EXPORTING list_display = abap_false
                            IMPORTING  r_salv_table   = lo_salv_table
                            CHANGING   t_table        = lt_salv_1[]  ).
  CATCH cx_salv_msg.
ENDTRY.

lo_salv_table->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
lo_salv_table->get_layout( )->set_default( abap_true ).
lo_salv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
lo_salv_table->get_functions( )->set_all( abap_true ).
lo_display = lo_salv_table->get_display_settings( ).
lo_display->set_striped_pattern( cl_salv_display_settings=>true ).


DATA columns TYPE REF TO cl_salv_columns_table.
columns = lo_salv_table->get_columns( ).
columns->set_optimize( ).

CALL METHOD lo_salv_table->display.