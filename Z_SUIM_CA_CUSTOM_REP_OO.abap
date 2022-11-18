***********************************************************************
* Report Z_SUIM_CA_CUSTOM_REP_OO                                      *
*                                                                     *
***********************************************************************
* Created on: 16.11.2022                                              *
* Created by: Jakub Walczak (jakub.walczak@lingarogroup.com)          *
*                                                                     *
***********************************************************************
REPORT z_suim_ca_custom_rep_oo.

CLASS lcl_salv_model DEFINITION INHERITING FROM cl_salv_model_list.
  PUBLIC SECTION.
    DATA: lo_control TYPE REF TO cl_salv_controller_model,
          lo_adapter TYPE REF TO cl_salv_adapter,
          lo_model TYPE REF TO cl_salv_model.
    METHODS:
      grabe_model
        IMPORTING
          io_model TYPE REF TO cl_salv_model,
      grabe_controller,
      grabe_adapter.
ENDCLASS.
*----------------------------------------------------------------------*
* Event handler for the added buttons
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
          lo_full_adap TYPE REF TO cl_salv_fullscreen_adapter,
          ls_layout    TYPE lvc_s_layo,
          ls_fieldcat  TYPE lvc_t_fcat.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.
*----------------------------------------------------------------------*
* Local Report class - Definition
*----------------------------------------------------------------------*

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.

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

             BEGIN OF __ty_rsusr200,
               bname       TYPE xubname,
               erdat       TYPE xuerdat,
               trdat       TYPE xuldate_alv,
               ltime       TYPE xultime,
               icon_locked TYPE xuuflag_alv,
               lock_reason TYPE xuureason_alv,
               usr02flag   TYPE xuuflag,
             END OF __ty_rsusr200,

             BEGIN OF __ty_salv_1,
               sysname       TYPE c LENGTH 10,
               systemid      TYPE sy-sysid,
               auth_id       TYPE uscraut-auth_id,
               text          TYPE uscrauidt-text,
               bname         TYPE ususerall-bname,
               name_text     TYPE ususerall-name_text,
               class         TYPE ususerall-class,
               gltgv         TYPE ususerall-gltgv,
               gltgb         TYPE ususerall-gltgb,
               accnt         TYPE ususerall-accnt,
               ustyp         TYPE ususerall-ustyp,
               erdat         TYPE xuerdat,
               trdat         TYPE xuldate,
               ltime         TYPE xultime,
               icon_locked   TYPE xuuflag_alv,
               lock_reason   TYPE xuureason_alv,
               init_analysis TYPE string,
               approval      TYPE flag,
               comment       TYPE string,
               ticket        TYPE string,
             END OF __ty_salv_1.

    DATA :lt_ca         TYPE   STANDARD TABLE OF __ty_ca,
          lt_rsusr200   TYPE   STANDARD TABLE OF __ty_rsusr200,
          lt_salv_1     TYPE   STANDARD TABLE OF __ty_salv_1,
          wa_ca         TYPE __ty_ca,
          wa_rsusr200   TYPE __ty_rsusr200,
          wa_salv_1     TYPE __ty_salv_1,
          lr_data       TYPE REF TO data,
          lo_salv_table TYPE REF TO cl_salv_table,
          lo_salv_model TYPE REF TO lcl_salv_model,
          lo_functions  TYPE REF TO cl_salv_functions,
          lo_columns    TYPE REF TO cl_salv_columns_table,
          lo_column     TYPE REF TO cl_salv_column,
          lo_col_list   TYPE REF TO cl_salv_column_list,
          lo_events     TYPE REF TO cl_salv_events_table,
          lo_event_h    TYPE REF TO lcl_event_handler,
          lo_alv_mod    TYPE REF TO cl_salv_model,
          lv_text       TYPE string,
          lv_icon       TYPE string.

    METHODS:
      generate_output.
ENDCLASS.

DATA: lo_report TYPE REF TO lcl_report.

START-OF-SELECTION.
  CREATE OBJECT lo_report.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: var_id TYPE c LENGTH 30 DEFAULT 'ZARDAGH_ACE_REPORTS'.
  SELECTION-SCREEN END OF BLOCK b1.
  lo_report->generate_output( ).

CLASS lcl_report IMPLEMENTATION.
  METHOD generate_output.

    FIELD-SYMBOLS  : <lt_data> TYPE ANY TABLE,
                     <lt_tab>  TYPE any.

    DATA: lv_systype(10) TYPE c.

    SELECT  name  FROM swfeature
      ORDER BY mod_date
      INTO @lv_systype
      UP TO 1 ROWS.
    ENDSELECT.

    cl_salv_bs_runtime_info=>set(
    EXPORTING
      display  = abap_false
      metadata = abap_false
      data     = abap_true
    ).

    CALL FUNCTION 'LIST_FREE_MEMORY'.

    " capture output from 'Users/Roles with Combinations of Critical Authorizations'
    SUBMIT rsusr008_009_new
    WITH comb = ''
    WITH auth = 'X'
    WITH authvar = var_id
    WITH d_analys = 'X'
    EXPORTING LIST TO MEMORY
    AND RETURN.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
        ASSIGN lr_data->* TO <lt_data>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE TEXT-M02 TYPE 'E'.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).

    LOOP AT <lt_data> ASSIGNING <lt_tab> .
      MOVE-CORRESPONDING EXACT <lt_tab> TO wa_ca EXPANDING NESTED TABLES.
      APPEND wa_ca TO lt_ca.
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

    " capture output from 'List of Users According to Logon Date and Password Change'
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
        MESSAGE TEXT-M02 TYPE 'E'.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).

    LOOP AT <lt_data> ASSIGNING  <lt_tab>.
      MOVE-CORRESPONDING  EXACT <lt_tab> TO wa_rsusr200 EXPANDING NESTED TABLES.
      APPEND wa_rsusr200 TO lt_rsusr200.
    ENDLOOP.

    CALL FUNCTION 'LIST_FREE_MEMORY'.

    REFRESH <lt_data>.
    FREE <lt_data>.
    CLEAR lr_data.

    " join 2 internal tables
    IF lt_ca[] IS NOT INITIAL
    AND lt_rsusr200[] IS NOT INITIAL.
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

        CLEAR: wa_rsusr200.
        READ TABLE lt_rsusr200 INTO wa_rsusr200 WITH KEY bname = wa_ca-bname.

        wa_salv_1-erdat = wa_rsusr200-erdat.
        wa_salv_1-trdat = wa_rsusr200-trdat.
        wa_salv_1-ltime = wa_rsusr200-ltime.
        wa_salv_1-icon_locked = wa_rsusr200-icon_locked.
        wa_salv_1-lock_reason = wa_rsusr200-lock_reason.
        wa_salv_1-init_analysis = ''.
        wa_salv_1-approval = ''.
        wa_salv_1-comment = ''.
        wa_salv_1-ticket = ''.

        APPEND wa_salv_1 TO lt_salv_1.

      ENDLOOP.
    ENDIF.

    SORT lt_salv_1 BY auth_id bname.

    " prepare SALV
    TRY.
        cl_salv_table=>factory( EXPORTING
                                        list_display = abap_false
                                        container_name = 'CONTAINER'
                                IMPORTING  r_salv_table   = lo_salv_table
                                CHANGING   t_table        = lt_salv_1  ).
      CATCH cx_salv_msg.
    ENDTRY.

    lo_columns = lo_salv_table->get_columns( ).
    lo_columns->set_optimize( ).

    " Change the properties of the columns
    TRY.
        lo_column = lo_columns->get_column( 'SYSNAME' ).
        lo_column->set_long_text( 'System name' ).
        lo_column = lo_columns->get_column( 'SYSTEMID' ).
        lo_column->set_long_text( 'System ID' ).
        lo_column = lo_columns->get_column( 'AUTH_ID' ).
        lo_column->set_long_text( 'ID of Critical Authorization (CA)' ).
        lo_column = lo_columns->get_column( 'TEXT' ).
        lo_column->set_long_text( 'Text of Critical Authorization (CA)' ).
        lo_column = lo_columns->get_column( 'BNAME' ).
        lo_column->set_long_text( 'User Name' ).
        lo_column = lo_columns->get_column( 'NAME_TEXT' ).
        lo_column->set_long_text( 'Full Name' ).
        lo_column = lo_columns->get_column( 'CLASS' ).
        lo_column->set_long_text( 'User Group (General)' ).
        lo_column = lo_columns->get_column( 'ERDAT' ).
        lo_column->set_long_text( 'Creation Date' ).
        lo_column = lo_columns->get_column( 'INIT_ANALYSIS' ).
        lo_column->set_long_text( 'Initial analysis' ).
        lo_column = lo_columns->get_column( 'APPROVAL' ).
        lo_column->set_short_text( 'Approval' ).
        lo_col_list ?= lo_columns->get_column( 'APPROVAL' ).
        lo_col_list->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        lo_col_list ?= lo_columns->get_column( 'ICON_LOCKED' ).
        lo_col_list->set_icon( if_salv_c_bool_sap=>true ).
        lo_column = lo_columns->get_column( 'COMMENT' ).
        lo_column->set_long_text( 'Comments/Remedial Actions' ).
        lo_column = lo_columns->get_column( 'TICKET' ).
        lo_column->set_long_text( 'Follow-Up Ticket' ).
        CLEAR: lo_column, lo_col_list.

      CATCH cx_salv_not_found.
    ENDTRY.

    lo_salv_table->set_screen_status(
      pfstatus      =  'ZCAREPSTATUS'
      report        =  sy-repid
      set_functions = lo_salv_table->c_functions_all ).

    lo_events = lo_salv_table->get_event( ).
    CREATE OBJECT lo_event_h.
    SET HANDLER lo_event_h->on_user_command FOR lo_events.
    SET HANDLER lo_event_h->on_link_click FOR lo_events.

    lo_alv_mod ?= lo_salv_table.
    CREATE OBJECT lo_salv_model.
    CALL METHOD lo_salv_model->grabe_model
      EXPORTING
        io_model = lo_alv_mod.

    lo_salv_table->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
    lo_salv_table->get_layout( )->set_default( abap_true ).
    lo_salv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_functions = lo_salv_table->get_functions( ).
    lo_functions->set_all( abap_true ).
    lo_salv_table->display( ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_salv_model IMPLEMENTATION.
  METHOD grabe_model.
    lo_model = io_model.
  ENDMETHOD.                    "grabe_model
  METHOD grabe_controller.
    lo_control = lo_model->r_controller.
  ENDMETHOD.                    "grabe_controller
  METHOD grabe_adapter.
    lo_adapter ?= lo_model->r_controller->r_adapter.
  ENDMETHOD.                    "grabe_adapter
ENDCLASS.
*----------------------------------------------------------------------*
* Event Handler for the SALV
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_user_command.
    FIELD-SYMBOLS <fs_alv_fieldcat> LIKE LINE OF ls_fieldcat.
    ls_layout-cwidth_opt = 'X'.

    CASE e_salv_function.
        " Make ALV as Editable ALV
      WHEN 'CHANGE'.
        CALL METHOD lo_report->lo_salv_model->grabe_controller.
        CALL METHOD lo_report->lo_salv_model->grabe_adapter.
        lo_full_adap ?= lo_report->lo_salv_model->lo_adapter.
        lo_grid = lo_full_adap->get_grid( ).
        lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
        lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
        IF lo_grid IS BOUND.
          " Editable ALV
*          ls_layout-edit = 'X'.
          CALL METHOD lo_grid->get_frontend_fieldcatalog
            IMPORTING
              et_fieldcatalog = ls_fieldcat.
          LOOP AT ls_fieldcat ASSIGNING <fs_alv_fieldcat>.
            CASE <fs_alv_fieldcat>-fieldname.
              WHEN 'INIT_ANALYSIS' OR 'COMMENT' OR 'TICKET'.
                <fs_alv_fieldcat>-edit = 'X'.
              WHEN 'APPROVAL'.
                <fs_alv_fieldcat>-edit = 'X'.
                <fs_alv_fieldcat>-checkbox = 'X'.
                <fs_alv_fieldcat>-tooltip = 'Approval'.
            ENDCASE.
          ENDLOOP.
          CALL METHOD lo_grid->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = ls_fieldcat.
          CALL METHOD lo_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.
          " refresh the table
          CALL METHOD lo_grid->refresh_table_display.
        ENDIF.
      WHEN 'SAVE'.
        IF lo_grid IS BOUND.
          CALL METHOD lo_report->lo_salv_model->grabe_controller.
          CALL METHOD lo_report->lo_salv_model->grabe_adapter.
          lo_full_adap ?= lo_report->lo_salv_model->lo_adapter.
          lo_grid = lo_full_adap->get_grid( ).
          lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
          lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
          LOOP AT ls_fieldcat ASSIGNING <fs_alv_fieldcat>.
            CLEAR <fs_alv_fieldcat>-edit.
          ENDLOOP.
          CALL METHOD lo_grid->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = ls_fieldcat.
          CALL METHOD lo_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.
          CALL METHOD lo_grid->refresh_table_display.
          MESSAGE TEXT-m01 TYPE 'I'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD on_link_click.

    FIELD-SYMBOLS: <lfa_data> LIKE LINE OF lo_report->lt_salv_1.
    READ TABLE lo_report->lt_salv_1 ASSIGNING <lfa_data> INDEX row.
    CHECK sy-subrc IS INITIAL.
    IF <lfa_data>-approval IS INITIAL.
      <lfa_data>-approval = 'X'.
    ELSE.
      CLEAR <lfa_data>-approval.
    ENDIF.
    APPEND <lfa_data>  TO lo_report->lt_salv_1.
    lo_report->lo_salv_table->refresh( ).
  ENDMETHOD.

ENDCLASS.