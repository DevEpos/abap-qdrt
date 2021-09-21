"! <p class="shorttext synchronized" lang="en">Exception for application errors</p>
CLASS zcx_qdrt_appl_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_t100_message,
      zif_qdrt_exception_message.

    ALIASES:
      get_message FOR zif_qdrt_exception_message~get_message.

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error.

    DATA:
      msgv1 TYPE sy-msgv1 READ-ONLY,
      msgv2 TYPE sy-msgv2 READ-ONLY,
      msgv3 TYPE sy-msgv3 READ-ONLY,
      msgv4 TYPE sy-msgv4 READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          text           TYPE string OPTIONAL
          status         TYPE i DEFAULT cl_rest_status_code=>gc_client_error_bad_request
          save_callstack TYPE abap_bool DEFAULT abap_true
          textid         LIKE if_t100_message=>t100key OPTIONAL
          previous       LIKE previous OPTIONAL
          msgv1          TYPE sy-msgv1 OPTIONAL
          msgv2          TYPE sy-msgv2 OPTIONAL
          msgv3          TYPE sy-msgv3 OPTIONAL
          msgv4          TYPE sy-msgv4 OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
      status    FOR zif_qdrt_exception_message~http_status,
      callstack FOR zif_qdrt_exception_message~callstack.

    METHODS:
      save_callstack.
ENDCLASS.



CLASS zcx_qdrt_appl_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: fill_t100_from_sy TYPE abap_bool.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.
    me->status = status.

    IF text IS NOT INITIAL.
      fill_t100_from_sy = abap_true.
      zcl_qdrt_message_helper=>set_msg_vars_from_text( text ).
    ELSEIF sy-msgid IS NOT INITIAL.
      fill_t100_from_sy = abap_true.
    ELSEIF textid IS NOT INITIAL.
      if_t100_message~t100key = textid.
      me->msgv1 = msgv1.
      me->msgv2 = msgv2.
      me->msgv3 = msgv3.
      me->msgv4 = msgv4.
    ELSE.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ENDIF.

    IF fill_t100_from_sy = abap_true.
      me->msgv1 = sy-msgv1.
      me->msgv2 = sy-msgv2.
      me->msgv3 = sy-msgv3.
      me->msgv4 = sy-msgv4.
      if_t100_message~t100key = VALUE #(
        msgid = sy-msgid
        msgno = sy-msgno
        attr1 = 'MSGV1'
        attr2 = 'MSGV2'
        attr3 = 'MSGV3'
        attr4 = 'MSGV4' ).
    ENDIF.

    IF save_callstack = abap_true.
      save_callstack( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_qdrt_exception_message~get_message.
    result = zcl_qdrt_message_helper=>get_exc_message(
      textid       = if_t100_message~t100key
      previous_exc = previous
      exc_message  = me
      msgv1        = msgv1
      msgv2        = msgv2
      msgv3        = msgv3
      msgv4        = msgv4 ).
  ENDMETHOD.


  METHOD save_callstack.
    DATA:
      l_callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = l_callstack.

    LOOP AT l_callstack ASSIGNING FIELD-SYMBOL(<callstack_line>).
      " skipfirst line in stack as it is this method call
      CHECK <callstack_line>-mainprogram NP 'ZCX_QDRT*'.

      " skip all stack lines starting from CL_REST_RESOURCE as they are no longer
      "  specific to this application
      IF <callstack_line>-mainprogram CP 'CL_REST_RESOURCE*'.
        EXIT.
      ENDIF.

      APPEND <callstack_line> TO callstack.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
