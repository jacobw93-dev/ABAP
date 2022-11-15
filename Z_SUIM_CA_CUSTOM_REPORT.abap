*&---------------------------------------------------------------------*
*& Report z_suim_ca_custom
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SUIM_CA_CUSTOM_REPORT.

TYPES  : BEGIN OF w_tab_ca,
           auth_id   TYPE uscraut-auth_id,
           text      TYPE uscrauidt-text,
           bname     TYPE ususerall-bname,
           name_text TYPE ususerall-name_text,
           class     TYPE ususerall-class,
           gltgv     TYPE ususerall-gltgv,
           gltgb     TYPE ususerall-gltgb,
           accnt     TYPE ususerall-accnt,
           ustyp     TYPE ususerall-ustyp,
         END OF w_tab_ca ,

         BEGIN OF w_tab_rsusr200,
           bname       TYPE xubname,
           erdat       TYPE xuerdat,
           trdat       TYPE xuldate_alv,
           ltime       TYPE xultime,
           icon_locked TYPE xuuflag_alv,
           lock_reason TYPE xuureason_alv,
           usr02flag   TYPE xuuflag,
         END OF w_tab_rsusr200,

         BEGIN OF w_outtab,
           auth_id     TYPE uscraut-auth_id,
           text        TYPE uscrauidt-text,
           bname       TYPE ususerall-bname,
           name_text   TYPE ususerall-name_text,
           class       TYPE ususerall-class,
           gltgv       TYPE ususerall-gltgv,
           gltgb       TYPE ususerall-gltgb,
           accnt       TYPE ususerall-accnt,
           ustyp       TYPE ususerall-ustyp,
           erdat       TYPE xuerdat,
           trdat       TYPE xuldate_alv,
           ltime       TYPE xultime,
           icon_locked TYPE xuuflag_alv,
           lock_reason TYPE xuureason_alv,
           usr02flag   TYPE xuuflag,
         END OF w_outtab.

FIELD-SYMBOLS  : <lt_data> TYPE ANY TABLE,
                 <lt_tab>  TYPE any.

DATA :it_tab_ca       TYPE   STANDARD TABLE OF w_tab_ca WITH HEADER LINE,
      it_tab_rsusr200 TYPE   STANDARD TABLE OF w_tab_rsusr200 WITH HEADER LINE,
      it_outtab       TYPE   STANDARD TABLE OF w_outtab WITH HEADER LINE,
      wa_tab_ca       TYPE w_tab_ca,
      wa_tab_rsusr200 TYPE w_tab_rsusr200,
      wa_outtab       TYPE w_outtab,
      lr_data         TYPE REF TO data,
      gr_table        TYPE REF TO cl_salv_table.

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
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
    ASSIGN lr_data->* TO <lt_data>.
  CATCH cx_salv_bs_sc_runtime_info.
    MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
ENDTRY.

cl_salv_bs_runtime_info=>clear_all( ).

LOOP AT <lt_data> ASSIGNING  <lt_tab>.
  MOVE-CORRESPONDING  <lt_tab> TO it_tab_ca.
  APPEND it_tab_ca.
ENDLOOP.


REFRESH <lt_data>.
FREE <lt_data>.
CLEAR lr_data.

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
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
    ASSIGN lr_data->* TO <lt_data>.
  CATCH cx_salv_bs_sc_runtime_info.
    MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
ENDTRY.

cl_salv_bs_runtime_info=>clear_all( ).

LOOP AT <lt_data> ASSIGNING  <lt_tab>.
  MOVE-CORRESPONDING  <lt_tab> TO it_tab_rsusr200.
  APPEND it_tab_rsusr200.
ENDLOOP.

CALL FUNCTION 'LIST_FREE_MEMORY'.

REFRESH <lt_data>.
FREE <lt_data>.
CLEAR lr_data.

IF it_tab_ca[] IS NOT INITIAL
AND it_tab_rsusr200[] IS NOT INITIAL.
  LOOP AT  it_tab_ca INTO wa_tab_ca.

    wa_outtab-auth_id = wa_tab_ca-auth_id.
    wa_outtab-text = wa_tab_ca-text.
    wa_outtab-bname = wa_tab_ca-bname.
    wa_outtab-name_text = wa_tab_ca-name_text.
    wa_outtab-class = wa_tab_ca-class.
    wa_outtab-gltgv = wa_tab_ca-gltgv.
    wa_outtab-gltgb = wa_tab_ca-gltgb.
    wa_outtab-accnt = wa_tab_ca-accnt.
    wa_outtab-ustyp = wa_tab_ca-ustyp.

    CLEAR: wa_tab_rsusr200.
    READ TABLE it_tab_rsusr200 INTO wa_tab_rsusr200 WITH KEY bname = wa_tab_ca-bname.

    wa_outtab-erdat = wa_tab_rsusr200-erdat.
    wa_outtab-trdat = wa_tab_rsusr200-trdat.
    wa_outtab-ltime = wa_tab_rsusr200-ltime.
    wa_outtab-icon_locked = wa_tab_rsusr200-icon_locked.
    wa_outtab-lock_reason = wa_tab_rsusr200-lock_reason.
    wa_outtab-usr02flag = wa_tab_rsusr200-usr02flag.

    APPEND wa_outtab TO it_outtab.

  ENDLOOP.
ENDIF.

SORT it_outtab BY auth_id bname ASCENDING.

TRY.
    cl_salv_table=>factory( IMPORTING  r_salv_table   = gr_table
                            CHANGING   t_table        = it_outtab[]  ).
  CATCH cx_salv_msg.
ENDTRY.

gr_table->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
gr_table->get_layout( )->set_default( abap_true ).
gr_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
gr_table->get_functions( )->set_all( abap_true ).

DATA columns TYPE REF TO cl_salv_columns_table.
columns = gr_table->get_columns( ).
columns->set_optimize( ).

CALL METHOD gr_table->display.