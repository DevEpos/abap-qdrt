"! <p class="shorttext synchronized" lang="en">Data selector for CDS view</p>
CLASS zcl_qdrt_cds_data_selector DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_data_selector_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_data_selector.

    METHODS:
      constructor
        IMPORTING
          name                  TYPE zif_qdrt_ty_global=>ty_entity_name
          type                  TYPE zif_qdrt_ty_global=>ty_entity_type
          filter_provider       TYPE REF TO zif_qdrt_filter_provider
          param_filter_provider TYPE REF TO zif_qdrt_filter_provider OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      param_filter_provider TYPE REF TO zif_qdrt_filter_provider.
ENDCLASS.



CLASS zcl_qdrt_cds_data_selector IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      name            = name
      type            = type
      filter_provider = filter_provider ).

    me->param_filter_provider = param_filter_provider.

  ENDMETHOD.


  METHOD zif_qdrt_data_selector~select_data.

  ENDMETHOD.


ENDCLASS.
