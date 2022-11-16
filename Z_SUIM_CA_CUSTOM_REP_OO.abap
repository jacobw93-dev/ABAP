*&---------------------------------------------------------------------*
*& Report Z_SUIM_CA_CUSTOM_REP_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_suim_ca_custom_rep_oo.

CLASS lcl_salv_model DEFINITION INHERITING FROM cl_salv_model_list.
  PUBLIC SECTION.
    DATA: o_control  TYPE REF TO cl_salv_controller_model,
          lo_adapter TYPE REF TO cl_salv_adapter.
    METHODS:
      grabe_model
        IMPORTING
          io_model TYPE REF TO cl_salv_model,
      grabe_controller,
      grabe_adapter.
  PRIVATE SECTION.
    DATA: lo_model TYPE REF TO cl_salv_model.
ENDCLASS.                    "LCL_SALV_MODEL DEFINITION
*----------------------------------------------------------------------*
* Event handler for the added buttons
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
* Local Report class - Definition
*----------------------------------------------------------------------*

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.

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
               trdat       TYPE XULDATE,
               ltime       TYPE xultime,
               icon_locked TYPE xuuflag_alv,
               lock_reason TYPE xuureason_alv,
               COMMENT      TYPE COMMENT,
             END OF w_outtab.

    DATA :it_tab_ca       TYPE   STANDARD TABLE OF w_tab_ca,
          it_tab_rsusr200 TYPE   STANDARD TABLE OF w_tab_rsusr200,
          it_outtab       TYPE   STANDARD TABLE OF w_outtab,
          wa_tab_ca       TYPE w_tab_ca,
          wa_tab_rsusr200 TYPE w_tab_rsusr200,
          wa_outtab       TYPE w_outtab,
          lr_data         TYPE REF TO data.

    DATA: lo_salv       TYPE REF TO cl_salv_table.
    DATA: lo_salv_model TYPE REF TO lcl_salv_model.

    DATA: lr_functions TYPE REF TO cl_salv_functions,
          l_text       TYPE string,
          l_icon       TYPE string.


    METHODS:
      generate_output.
ENDCLASS.

DATA: lo_report TYPE REF TO lcl_report.

START-OF-SELECTION.
  CREATE OBJECT lo_report.
  lo_report->generate_output( ).

CLASS lcl_report IMPLEMENTATION.
  METHOD generate_output.

    FIELD-SYMBOLS  : <lt_data> TYPE ANY TABLE,
                     <lt_tab>  TYPE any.

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

    LOOP AT <lt_data> ASSIGNING <lt_tab> .
      MOVE-CORRESPONDING EXACT <lt_tab> TO wa_tab_ca EXPANDING NESTED TABLES.
      APPEND wa_tab_ca TO it_tab_ca.
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
      MOVE-CORRESPONDING  EXACT <lt_tab> TO wa_tab_rsusr200 EXPANDING NESTED TABLES.
      APPEND wa_tab_rsusr200 TO it_tab_rsusr200.
    ENDLOOP.

    CALL FUNCTION 'LIST_FREE_MEMORY'.

    REFRESH <lt_data>.
    FREE <lt_data>.
    CLEAR lr_data.
    data: v_dats type dats.

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

*        v_dats = wa_tab_rsusr200-trdat.
        wa_outtab-erdat = wa_tab_rsusr200-erdat.
        wa_outtab-trdat = wa_tab_rsusr200-trdat.
        wa_outtab-ltime = wa_tab_rsusr200-ltime.
        wa_outtab-icon_locked = wa_tab_rsusr200-icon_locked.
        wa_outtab-lock_reason = wa_tab_rsusr200-lock_reason.
        wa_outtab-COMMENT = ''.

        APPEND wa_outtab TO it_outtab.

      ENDLOOP.
    ENDIF.

    SORT it_outtab BY auth_id bname.

    TRY.
        cl_salv_table=>factory( EXPORTING
                                        list_display = abap_false
*                                        r_container  = container
*                                        container_name = 'CONTAINER'
                                IMPORTING  r_salv_table   = lo_salv
                                CHANGING   t_table        = it_outtab  ).
      CATCH cx_salv_msg.
    ENDTRY.

*    l_text = 'Edit'.
*    l_icon = icon_edit_file.
*
*    TRY.
*        lr_functions->add_function(
*          name     = 'EDIT'
*          icon     = l_icon
*          text     = l_text
*          tooltip  = l_text
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*      CATCH cx_salv_existing cx_salv_wrong_call.
*    ENDTRY.

    DATA: columns TYPE REF TO cl_salv_columns_table.
    columns = lo_salv->get_columns( ).
    columns->set_optimize( ).

    lo_salv->set_screen_status(
      pfstatus      =  'SALV_STANDARD'
      report        =  'SALV_DEMO_TABLE_EVENTS'
      set_functions = lo_salv->c_functions_all ).

    DATA: lo_events  TYPE REF TO cl_salv_events_table,
          lo_event_h TYPE REF TO lcl_event_handler.
    lo_events = lo_salv->get_event( ).
    CREATE OBJECT lo_event_h.
    SET HANDLER lo_event_h->on_user_command FOR lo_events.
    DATA: lo_alv_mod TYPE REF TO cl_salv_model.
    lo_alv_mod ?= lo_salv.
    CREATE OBJECT lo_salv_model.
    CALL METHOD lo_salv_model->grabe_model
      EXPORTING
        io_model = lo_alv_mod.

    lo_salv->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
    lo_salv->get_layout( )->set_default( abap_true ).
    lo_salv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lr_functions = lo_salv->get_functions( ).
    lr_functions->set_all( abap_true ).

    lo_salv->display( ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_salv_model IMPLEMENTATION.
  METHOD grabe_model.
    lo_model = io_model.
  ENDMETHOD.                    "grabe_model
  METHOD grabe_controller.
    o_control = lo_model->r_controller.
  ENDMETHOD.                    "grabe_controller
  METHOD grabe_adapter.
    lo_adapter ?= lo_model->r_controller->r_adapter.
  ENDMETHOD.                    "grabe_adapter
ENDCLASS.                    "LCL_SALV_MODEL IMPLEMENTATION
*----------------------------------------------------------------------*
* Event Handler for the SALV
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_user_command.
    DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
          lo_full_adap TYPE REF TO cl_salv_fullscreen_adapter.

    DATA: ls_layout   TYPE lvc_s_layo,
          ls_fieldcat TYPE lvc_t_fcat.


    FIELD-SYMBOLS <fs_alv_fieldcat> LIKE LINE OF ls_fieldcat.

    CASE e_salv_function.
*     Make ALV as Editable ALV
      WHEN 'MYFUNCTION'.
        CALL METHOD lo_report->lo_salv_model->grabe_controller.
        CALL METHOD lo_report->lo_salv_model->grabe_adapter.
        lo_full_adap ?= lo_report->lo_salv_model->lo_adapter.
        lo_grid = lo_full_adap->get_grid( ).
        lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
        lo_grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
        SET HANDLER handle_data_changed FOR lo_grid.

        IF lo_grid IS BOUND.
*         Editable ALV
*          ls_layout-edit = 'X'.
          ls_layout-cwidth_opt = 'X'.
          CALL METHOD lo_grid->get_frontend_fieldcatalog
            IMPORTING
              et_fieldcatalog = ls_fieldcat.

          LOOP AT ls_fieldcat ASSIGNING <fs_alv_fieldcat>.
            IF <fs_alv_fieldcat>-fieldname = 'COMMENT'.
              <fs_alv_fieldcat>-coltext = 'Initial analysis'.
              IF <fs_alv_fieldcat>-edit = abap_false.
                <fs_alv_fieldcat>-edit = 'X'.
              ELSE.
                CLEAR <fs_alv_fieldcat>-edit.
              ENDIF.
            ENDIF.
          ENDLOOP.

          CALL METHOD lo_grid->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = ls_fieldcat.

          CALL METHOD lo_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.

*         refresh the table
          CALL METHOD lo_grid->refresh_table_display.
        ENDIF.
      WHEN 'SAVE'.
        MESSAGE 'Here we save "t_data" to database' TYPE 'W'.
    ENDCASE.
  ENDMETHOD.                    "on_user_command
  METHOD handle_data_changed.

    " Here we can print changed data for example
    DATA: lt_mod_cells TYPE lvc_t_modi,
          ls_mod_cells TYPE lvc_s_modi,
          lv_message   TYPE string.
    FIELD-SYMBOLS: <fs_mod_cells> TYPE lvc_s_modi,
                   <ft_mod_rows>  TYPE table,
                   <fs_mod_rows>  TYPE any,
                   <fs>           TYPE any.

    ASSIGN er_data_changed->mp_mod_rows->* TO <ft_mod_rows>.

    READ TABLE <ft_mod_rows> ASSIGNING <fs_mod_rows> INDEX 1.
    ASSIGN COMPONENT 'COMMENT' OF STRUCTURE <fs_mod_rows> TO <fs>.

    lt_mod_cells = er_data_changed->mt_mod_cells.
    READ TABLE lt_mod_cells INTO ls_mod_cells INDEX 1.

    lv_message = |Changed comments: { <fs> }; changed data: { ls_mod_cells-value }|.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION