"! <p class="shorttext synchronized" lang="en">SQL query error</p>
CLASS zcx_qdrt_sql_query_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_qdrt_appl_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA line_number TYPE i.
    DATA message TYPE string.

    CONSTANTS:
      BEGIN OF invalid_token_in_data_declare,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_token_in_data_declare .
    CONSTANTS:
      BEGIN OF invalid_statement,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_statement .
    CONSTANTS:
      BEGIN OF invalid_token,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_token .
    CONSTANTS:
      BEGIN OF too_many_select_stmnt,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF too_many_select_stmnt .
    CONSTANTS:
      BEGIN OF no_select_statement,
        msgid TYPE symsgid VALUE 'ZQDRT_ERROR',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_select_statement .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !text       TYPE string OPTIONAL
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !msgv1      TYPE sy-msgv1 OPTIONAL
        !msgv2      TYPE sy-msgv2 OPTIONAL
        !msgv3      TYPE sy-msgv3 OPTIONAL
        !msgv4      TYPE sy-msgv4 OPTIONAL
        line_number TYPE i OPTIONAL
        message     TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_qdrt_sql_query_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        text     = text
        previous = previous
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4.
    CLEAR me->textid.

    me->line_number = line_number.
    me->message = message.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
