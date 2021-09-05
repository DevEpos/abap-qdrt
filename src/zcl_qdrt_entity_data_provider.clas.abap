"! <p class="shorttext synchronized" lang="en">Data provider for DB entities</p>
CLASS zcl_qdrt_entity_data_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_query_config,
        filters TYPE zif_qdrt_filter_provider=>ty_filters,
      END OF ty_query_config.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance</p>
      constructor
        IMPORTING
          name         TYPE zif_qdrt_ty_global=>ty_entity_name
          type         TYPE zif_qdrt_ty_global=>ty_entity_type
          query_config TYPE ty_query_config
        RAISING
          zcx_qdrt_appl_error,

      "! <p class="shorttext synchronized" lang="en">Retrieves data</p>
      get_data
        RETURNING
          VALUE(result) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      entity_name   TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type   TYPE zif_qdrt_ty_global=>ty_entity_type,
      data_selector TYPE REF TO zif_qdrt_data_selector.

    METHODS:
      init_data_selector
        IMPORTING
          query_config  TYPE ty_query_config
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_data_selector
        RAISING
          zcx_qdrt_conversion_error.
ENDCLASS.



CLASS zcl_qdrt_entity_data_provider IMPLEMENTATION.


  METHOD constructor.
    entity_name = name.
    entity_type = type.
    data_selector = init_data_selector( query_config ).
  ENDMETHOD.


  METHOD get_data.
    result = data_selector->select_data( ).
  ENDMETHOD.


  METHOD init_data_selector.
    DATA: filter_provider TYPE REF TO zif_qdrt_filter_provider.

    DATA(field_filters) = query_config-filters.
    DELETE field_filters WHERE field_name CP |{ zif_qdrt_c_global=>c_param_filter_prefix }*|.


    DATA(metadata_provider) = zcl_qdrt_provider_factory=>create_entity_metadata(
      entity_name = entity_name
      entity_type = entity_type ).

    DATA(filter_converter) = zcl_qdrt_provider_factory=>create_filter_converter( ).
    filter_converter->convert(
      EXPORTING metadata_provider = metadata_provider
      CHANGING filters            = field_filters ).

    filter_provider = NEW zcl_qdrt_default_fp( field_filters ).

    CASE entity_type.

      WHEN zif_qdrt_c_entity_types=>cds_view.

        DATA(param_filters) = query_config-filters.
        DELETE param_filters WHERE field_name NP |{ zif_qdrt_c_global=>c_param_filter_prefix }*|.

        result = NEW zcl_qdrt_cds_data_selector(
          name                  = entity_name
          type                  = entity_type
          filter_provider       = filter_provider
          param_filter_provider = COND #( WHEN param_filters IS NOT INITIAL THEN
                                            NEW zcl_qdrt_cds_fp( param_filters ) ) ).

      WHEN zif_qdrt_c_entity_types=>database_table OR
           zif_qdrt_c_entity_types=>view.

        result = NEW zcl_qdrt_table_data_selector(
          name            = entity_name
          type            = entity_type
          filter_provider = filter_provider  ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
