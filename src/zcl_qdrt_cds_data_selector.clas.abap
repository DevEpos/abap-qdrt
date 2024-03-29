"! <p class="shorttext synchronized" lang="en">Data selector for CDS view</p>
CLASS zcl_qdrt_cds_data_selector DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_data_selector_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          name                  TYPE zif_qdrt_ty_global=>ty_entity_name
          type                  TYPE zif_qdrt_ty_global=>ty_entity_type
          metadata_provider     TYPE REF TO zif_qdrt_entity_metadata_prov
          filter_provider       TYPE REF TO zif_qdrt_filter_provider
          output_field_config   TYPE REF TO zif_qdrt_output_field_config
          sort_config           TYPE REF TO zif_qdrt_sort_config
          aggregation_config    TYPE REF TO zif_qdrt_aggregation_config
          param_filter_provider TYPE REF TO zif_qdrt_filter_provider OPTIONAL.

  PROTECTED SECTION.
    METHODS:
      get_from_clause REDEFINITION.
  PRIVATE SECTION.
    DATA:
      param_filter_provider TYPE REF TO zif_qdrt_filter_provider.


ENDCLASS.



CLASS zcl_qdrt_cds_data_selector IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      name                = name
      type                = type
      metadata_provider   = metadata_provider
      filter_provider     = filter_provider
      output_field_config = output_field_config
      sort_config         = sort_config
      aggregation_config  = aggregation_config ).

    me->param_filter_provider = param_filter_provider.

  ENDMETHOD.


  METHOD get_from_clause.
    IF param_filter_provider IS INITIAL OR param_filter_provider->is_empty( ).
      result = VALUE #( ( |{ entity_name }| ) ).
    ELSE.
      result = param_filter_provider->get_filter_string( ).
      IF result IS NOT INITIAL.
        ASSIGN result[ 1 ] TO FIELD-SYMBOL(<entity_line>).
        <entity_line> = |{ entity_name }{ <entity_line> }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
