"! <p class="shorttext synchronized" lang="en">Common error during selection</p>
CLASS zcx_qdrt_selection_common DEFINITION
  PUBLIC
  INHERITING FROM zcx_qdrt_appl_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !text     TYPE string OPTIONAL
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_qdrt_selection_common IMPLEMENTATION.


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
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
