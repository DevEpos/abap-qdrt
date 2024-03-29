"! <p class="shorttext synchronized" lang="en">Converts filter values in preperation for DB select</p>
INTERFACE zif_qdrt_filter_converter
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Converts filter values</p>
    convert
      IMPORTING
        metadata_provider TYPE REF TO zif_qdrt_entity_metadata_prov
        filter_type       TYPE zif_qdrt_ty_global=>ty_field_type DEFAULT zif_qdrt_c_global=>c_field_types-normal_field
      CHANGING
        filters           TYPE zif_qdrt_filter_provider=>ty_filters
      RAISING
        zcx_qdrt_conversion_error.
ENDINTERFACE.
