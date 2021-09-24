"! <p class="shorttext synchronized" lang="en">Text Utitlity</p>
CLASS zcl_qdrt_text_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_texts TYPE STANDARD TABLE OF seu_objtxt
        WITH KEY object obj_name
        WITH UNIQUE SORTED KEY obj_name COMPONENTS obj_name object,
      BEGIN OF ty_text_key,
        name TYPE sobj_name,
        type TYPE trobjtype,
      END OF ty_text_key,
      ty_text_keys TYPE STANDARD TABLE OF ty_text_key WITH KEY name type.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves object short text</p>
      get_short_text
        IMPORTING
          object_type   TYPE trobjtype
          object_name   TYPE sobj_name
        RETURNING
          VALUE(result) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Retrieve short texts for repository objects</p>
      get_short_texts
        IMPORTING
          keys          TYPE ty_text_keys
        RETURNING
          VALUE(result) TYPE ty_texts.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_qdrt_text_util IMPLEMENTATION.


  METHOD get_short_text.
    DATA:
      texts TYPE TABLE OF seu_objtxt.

    texts = VALUE #( ( object = object_type obj_name = object_name ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.

    result = texts[ 1 ]-stext.
  ENDMETHOD.


  METHOD get_short_texts.
    result = CORRESPONDING ty_texts( keys MAPPING object   = type
                                                  obj_name = name ).
    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = result.
  ENDMETHOD.

ENDCLASS.
