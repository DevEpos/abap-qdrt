"! <p class="shorttext synchronized" lang="en">Base data selector</p>
CLASS zcl_qdrt_data_selector_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      constructor
        IMPORTING
          name            TYPE zif_qdrt_ty_global=>ty_entity_name
          type            TYPE zif_qdrt_ty_global=>ty_entity_type
          filter_provider TYPE REF TO zif_qdrt_filter_provider.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_data_selector_base IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


ENDCLASS.
