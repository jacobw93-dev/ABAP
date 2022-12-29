CLASS ZCL_MANUAL_UPD_LOG DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_rspls_logging_on_save .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_manual_upd_log IMPLEMENTATION.


  METHOD if_rspls_logging_on_save~log_defined.
    CHECK i_infocube_name = 'CA_DTG01' .
    r_log_defined = rs_c_true.
  ENDMETHOD.


  METHOD if_rspls_logging_on_save~log_defined_db.
*    CHECK i_infocube_name = 'CA_DTG01' .
*  CHECK sy-uname = '<USERID>' . // use your user id only during development,
**Comment user id restriction later once you are done.
    e_log_defined_db = rs_c_false.
  ENDMETHOD.


  METHOD if_rspls_logging_on_save~log_structure.
    TYPES:
      BEGIN OF tn_s_tablnm,
        dsotabtype TYPE rsdsotabtype,
        name       TYPE rsoadsonm,
      END OF tn_s_tablnm .
    TYPES:
      tn_t_tablnm TYPE STANDARD TABLE OF tn_s_tablnm WITH DEFAULT KEY .
    DATA: l_s_map          LIKE LINE OF e_t_map,
          l_s_map_proposal TYPE if_rspls_logging_on_save=>tn_s_map_proposal.
    DATA: l_dsonm       TYPE rsdodsobject,
          l_t_iobj      TYPE STANDARD TABLE OF bapi6116io,
          l_s_iobj      LIKE LINE OF l_t_iobj,
          l_tn_t_tablnm TYPE tn_t_tablnm,
          l_struc_name  TYPE tabname,
          l_s_details   TYPE bapi6108,
          l_s_return    TYPE bapiret2.
** Here we need to update our InfoObject names which would be used for logging
    CONSTANTS:
      c_iobjnm_user   TYPE rsiobjnm VALUE '0USERNAME',
      c_iobjnm_time   TYPE rsiobjnm VALUE '0TIME',
      c_iobjnm_date   TYPE rsiobjnm VALUE '0DATE',
      c_iobjnm_saveid TYPE rsiobjnm VALUE 'GSAVEID'.

*
*** Plan Cube and Audit DSO Information.
    CASE i_infocube_name.
      WHEN 'CA_DTG01'.
        l_dsonm = 'CA_DTG01'.
    ENDCASE.

    e_structure_name = '/BIC/ACA_DTG012'.

    CLEAR e_t_map.


    SELECT
        @l_dsonm AS odsobject, POSITION as posit, 'X' AS keyflag, fieldname AS infoobject
    FROM DD03L
    WHERE TABNAME = @e_structure_name
    INTO TABLE @l_t_iobj.



    DATA: lv_string_length TYPE i.

    LOOP AT l_t_iobj INTO l_s_iobj.
      l_s_map-field_name = l_s_iobj-infoobject.
      IF l_s_iobj-infoobject CP '/BIC/*'.
        lv_string_length = strlen( l_s_iobj-infoobject ) - 5.
        l_s_iobj-infoobject = l_s_iobj-infoobject+5(lv_string_length).
      ELSEIF l_s_iobj-infoobject = 'DATE0'.
        l_s_iobj-infoobject = '0DATE'.
      ELSE.
        l_s_iobj-infoobject = '0' && l_s_iobj-infoobject.
      ENDIF.
      l_s_map-iobj_name = l_s_iobj-infoobject.

* get the field name in the dso structure
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        EXPORTING
          version    = rs_c_objvers-active
          infoobject = l_s_iobj-infoobject
        IMPORTING
          details    = l_s_details
          return     = l_s_return.
      l_s_map-field_name = l_s_details-fieldnm.
      APPEND l_s_map TO e_t_map.
    ENDLOOP.

* get the user name
    READ TABLE i_t_map_proposal INTO l_s_map_proposal
    WITH KEY field_type = if_rspls_logging_on_save=>n_c_field_type-user.
    IF sy-subrc IS INITIAL.
      l_s_map-iobj_name = l_s_map_proposal-iobj_name.
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        EXPORTING
          version    = rs_c_objvers-active
          infoobject = c_iobjnm_user
        IMPORTING
          details    = l_s_details
          return     = l_s_return.
      IF l_s_return IS INITIAL.
        l_s_map-field_name = l_s_details-fieldnm.
        APPEND l_s_map TO e_t_map.
      ENDIF.
    ENDIF.
* get the date
    READ TABLE i_t_map_proposal INTO l_s_map_proposal
    WITH KEY field_type = if_rspls_logging_on_save=>n_c_field_type-date.
    IF sy-subrc IS INITIAL.
      l_s_map-iobj_name = l_s_map_proposal-iobj_name.
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        EXPORTING
          version    = rs_c_objvers-active
          infoobject = c_iobjnm_date
        IMPORTING
          details    = l_s_details
          return     = l_s_return.
      IF l_s_return IS INITIAL.
        l_s_map-field_name = l_s_details-fieldnm.
        APPEND l_s_map TO e_t_map.
      ENDIF.
    ENDIF.
* get the time
    READ TABLE i_t_map_proposal INTO l_s_map_proposal
    WITH KEY field_type = if_rspls_logging_on_save=>n_c_field_type-time.
    IF sy-subrc IS INITIAL.
      l_s_map-iobj_name = l_s_map_proposal-iobj_name.
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        EXPORTING
          version    = rs_c_objvers-active
          infoobject = c_iobjnm_time
        IMPORTING
          details    = l_s_details
          return     = l_s_return.
      IF l_s_return IS INITIAL.
        l_s_map-field_name = l_s_details-fieldnm.
        APPEND l_s_map TO e_t_map.
      ENDIF.
    ENDIF.
* get the save id
    READ TABLE i_t_map_proposal INTO l_s_map_proposal
    WITH KEY field_type = if_rspls_logging_on_save=>n_c_field_type-saveid.
    IF sy-subrc IS INITIAL.
      l_s_map-iobj_name = l_s_map_proposal-iobj_name.
      CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
        EXPORTING
          version    = rs_c_objvers-active
          infoobject = c_iobjnm_saveid
        IMPORTING
          details    = l_s_details
          return     = l_s_return.
      IF l_s_return IS INITIAL.
        l_s_map-field_name = l_s_details-fieldnm.
        APPEND l_s_map TO e_t_map.
      ENDIF.
    ENDIF.

    DELETE e_t_map WHERE
          iobj_name = c_iobjnm_user OR
          iobj_name = c_iobjnm_date OR
          iobj_name = c_iobjnm_time OR
          iobj_name = c_iobjnm_saveid.


  ENDMETHOD.

  METHOD if_rspls_logging_on_save~log_write.

    DATA: l_dsonm TYPE rsoadsonm,
          lt_msg  TYPE rs_t_msg.

    CASE i_infocube_name.
      WHEN 'CA_DTG01'.
        l_dsonm = 'CA_DTG01C'.
    ENDCASE.

    CALL FUNCTION 'RSDSO_DU_WRITE_API'
      EXPORTING
        i_adsonm            = l_dsonm
        it_data             = i_t_logging_data
      IMPORTING
        et_msg              = lt_msg
      EXCEPTIONS
        write_failed        = 1
        datastore_not_found = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD if_rspls_logging_on_save~log_write_db.


  ENDMETHOD.
ENDCLASS.