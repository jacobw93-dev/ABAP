CLASS zcl_bw_qv_par DEFINITION
  PUBLIC
  INHERITING FROM zcl_bw_qv_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bw_qv_par IMPLEMENTATION.


  METHOD get.


    TYPES:
      BEGIN OF _ty_s_auth,
        value TYPE  rschavl,
        uname TYPE  eqeuname,
      END OF _ty_s_auth.

    DATA:
      lv_exists    TYPE abap_bool,
      ls_auth      TYPE _ty_s_auth,
      lt_auth      TYPE HASHED TABLE OF _ty_s_auth
                   WITH UNIQUE KEY value,
      ls_range     TYPE rsr_s_rangesid,
      ls_var_range TYPE rrs0_s_var_range,
      lt_values    TYPE TABLE OF string,
      lv_selection TYPE c LENGTH 30,
      lv_lock      TYPE c LENGTH 30,
      lv_user      TYPE c LENGTH 30,
      lv_plandso   TYPE c LENGTH 30,
      lv_message_1 TYPE c LENGTH 50,
      lv_message_2 TYPE c LENGTH 50,
      lt_selhash   TYPE rspls_t_selhash,
      lt_selcheck  TYPE rspls_t_selcheck.

    FIELD-SYMBOLS:
      <fs_auth>     TYPE _ty_s_auth,
      <fs_selhash>  TYPE rspls_s_selhash,
      <fs_selcheck> TYPE rspls_s_selcheck.

    IF i_step = 3.

      CLEAR: lt_values.

      LOOP AT i_t_var_range INTO ls_var_range
          WHERE vnam = 'GMMIM_0GN_R3_SSY01'.
        IF sy-subrc = 0.
          APPEND ls_var_range-low TO lt_values.
        ENDIF.
      ENDLOOP.
      SORT lt_values ASCENDING.
      CONCATENATE LINES OF lt_values INTO lv_selection SEPARATED BY ','.

      CASE i_s_rkb1d-infocube.
        WHEN 'CA_CRC01'.
          lv_plandso = 'CA_DTG01'.
        WHEN 'CA_CRC03'.
          lv_plandso = 'CA_DTG03'.
      ENDCASE.

      CALL METHOD cl_rspls_enq=>read_locks
        EXPORTING
          i_infoprov   = lv_plandso
          i_uname      = ''
        IMPORTING
          e_t_selhash  = lt_selhash
          e_t_selcheck = lt_selcheck
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc = 0
         AND ( lt_selhash IS NOT INITIAL ).
*         OR lt_selcheck IS NOT INITIAL ).

        CLEAR: lv_lock.
        DELETE lt_selcheck WHERE chanm <> '0GN_R3_SSY'.
        SORT lt_selcheck BY guid ASCENDING.
        SORT lt_selhash BY guid ASCENDING.

        LOOP AT lt_selhash ASSIGNING <fs_selhash> WHERE uname <> sy-uname.
          IF lv_user IS NOT INITIAL.
            CONCATENATE lv_user <fs_selhash>-uname INTO lv_user SEPARATED BY ','.
          ELSE.
            lv_user  = <fs_selhash>-uname.
          ENDIF.

          IF lt_selcheck IS NOT INITIAL.
            LOOP AT lt_selcheck ASSIGNING <fs_selcheck>
                 WHERE guid = <fs_selhash>-guid.

              IF lv_lock IS NOT INITIAL.
                CONCATENATE lv_lock <fs_selcheck>-low INTO lv_lock SEPARATED BY ','.
              ELSE.
                lv_lock  = <fs_selcheck>-low.
              ENDIF.

              ls_auth-uname = <fs_selhash>-uname.
              ls_auth-value = <fs_selcheck>-low.
              INSERT ls_auth INTO TABLE lt_auth.

            ENDLOOP.

          ELSE.
            ls_auth-uname = <fs_selhash>-uname.
            INSERT ls_auth INTO TABLE lt_auth.
          ENDIF.

        ENDLOOP.

        IF lv_selection IS INITIAL.
          LOOP AT lt_auth ASSIGNING <fs_auth> WHERE uname <> sy-uname.
            lv_exists = abap_true.
          ENDLOOP.
        ELSE.
          IF lt_auth[] IS NOT INITIAL.
            IF lt_selcheck[] IS NOT INITIAL.
              LOOP AT lt_auth ASSIGNING <fs_auth> WHERE uname <> sy-uname.
                LOOP AT lt_values INTO DATA(lv_value).
                  READ TABLE lt_auth WITH KEY value = lv_value TRANSPORTING NO FIELDS.
                  IF sy-subrc = 0.
                    lv_exists = abap_true.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDLOOP.
            ELSE.
              lv_exists = abap_true.
            ENDIF.
          ENDIF.

        ENDIF.

        IF sy-subrc = 0 AND lv_exists = abap_true.

          lv_message_1 = 'Lock by user(s)'.
          lv_message_2 = 'Active for Source System(s):'.
          IF lv_lock IS INITIAL.
            CONCATENATE lv_message_1 lv_user INTO lv_message_1 SEPARATED BY space.
            lv_message_2 = 'Active for all Source Systems'.
          ELSE.
            IF lv_lock IS NOT INITIAL.
              CONCATENATE lv_message_1 lv_user INTO lv_message_1 SEPARATED BY space.
              CONCATENATE lv_message_2 lv_lock INTO lv_message_2 SEPARATED BY space.
            ELSEIF lv_selection IS NOT INITIAL.
              CONCATENATE lv_message_1 lv_user INTO lv_message_1 SEPARATED BY space.
              CONCATENATE lv_message_2 lv_selection INTO lv_message_2 SEPARATED BY space.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'RRMS_MESSAGE_COLLECT_CONTINUE'.
          CALL FUNCTION 'RRMS_MESSAGE_HANDLING'
            EXPORTING
              i_class  = 'RSBBS'
              i_type   = 'W'
              i_number = '000'
              i_msgv1  = lv_message_1
              i_msgv2  = lv_message_2.

*        CALL FUNCTION 'RRMS_MESSAGES_SHOW'.
*        RETURN.
          RAISE EXCEPTION TYPE cx_rs_error.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.