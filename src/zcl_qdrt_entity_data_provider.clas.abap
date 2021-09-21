"! <p class="shorttext synchronized" lang="en">Data provider for DB entities</p>
CLASS zcl_qdrt_entity_data_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_query_config,
        settings      TYPE zif_qdrt_ty_global=>ty_query_exec_settings,
        filters       TYPE zif_qdrt_filter_provider=>ty_filters,
        parameters    TYPE zif_qdrt_filter_provider=>ty_filters,
        aggregations  TYPE zif_qdrt_aggregation_config=>ty_aggregation_config,
        sort_fields   TYPE zif_qdrt_sort_config=>ty_sort_fields,
        output_fields TYPE zif_qdrt_output_field_config=>ty_output_fields,
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
          VALUE(result) TYPE REF TO data
        RAISING
          zcx_qdrt_selection_common,
      "! <p class="shorttext synchronized" lang="en">Retrieves available row count</p>
      get_max_row_count
        RETURNING
          VALUE(result) TYPE zqdrt_no_of_lines
        RAISING
          zcx_qdrt_selection_common,
      "! <p class="shorttext synchronized" lang="en">Retrieves metadata of entity</p>
      get_metadata
        RETURNING
          VALUE(result) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      entity_name       TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type       TYPE zif_qdrt_ty_global=>ty_entity_type,
      data_selector     TYPE REF TO zif_qdrt_data_selector,
      exec_settings     TYPE zif_qdrt_ty_global=>ty_query_exec_settings,
      metadata_provider TYPE REF TO zif_qdrt_entity_metadata_prov.

    METHODS:
      init_data_selector
        IMPORTING
          query_config  TYPE ty_query_config
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_data_selector
        RAISING
          zcx_qdrt_appl_error.
ENDCLASS.



CLASS zcl_qdrt_entity_data_provider IMPLEMENTATION.


  METHOD constructor.
    entity_name = name.
    entity_type = type.
    exec_settings = COND #(
      WHEN query_config-settings IS NOT INITIAL THEN query_config-settings
      ELSE VALUE #( max_rows = 200 ) ).
    data_selector = init_data_selector( query_config ).
  ENDMETHOD.


  METHOD get_data.
    result = data_selector->select_data( exec_settings ).
  ENDMETHOD.

  METHOD get_max_row_count.
    result = data_selector->get_max_count( ).
  ENDMETHOD.

  METHOD get_metadata.
    result = metadata_provider->get_metadata( ).
  ENDMETHOD.


  METHOD init_data_selector.
    DATA: filter_provider TYPE REF TO zif_qdrt_filter_provider.

    metadata_provider = zcl_qdrt_provider_factory=>create_entity_metadata(
      entity_name = entity_name
      entity_type = entity_type ).

    DATA(field_filters) = query_config-filters.
    DATA(filter_converter) = zcl_qdrt_provider_factory=>create_filter_converter( ).
    filter_converter->convert(
      EXPORTING
        metadata_provider = metadata_provider
      CHANGING
        filters           = field_filters ).

    filter_provider = NEW zcl_qdrt_entity_fp( field_filters ).

    DATA(output_fields_config) = NEW zcl_qdrt_output_field_config( query_config-output_fields ).
    DATA(sort_config) = NEW zcl_qdrt_sort_config( query_config-sort_fields ).
    DATA(aggregation_config) = NEW zcl_qdrt_aggregation_config( query_config-aggregations ).

    CASE entity_type.

      WHEN zif_qdrt_c_entity_types=>cds_view.

        DATA(param_filters) = query_config-parameters.
        filter_converter->convert(
          EXPORTING
            metadata_provider = metadata_provider
            filter_type       = zif_qdrt_c_global=>c_field_types-parameter
          CHANGING
            filters           = param_filters ).

        result = NEW zcl_qdrt_cds_data_selector(
          name                  = entity_name
          type                  = entity_type
          metadata_provider     = metadata_provider
          aggregation_config    = aggregation_config
          sort_config           = sort_config
          output_field_config   = output_fields_config
          filter_provider       = filter_provider
          param_filter_provider = COND #( WHEN param_filters IS NOT INITIAL THEN
                                            NEW zcl_qdrt_cds_parameter_fp( param_filters ) ) ).

      WHEN zif_qdrt_c_entity_types=>database_table OR
           zif_qdrt_c_entity_types=>view.

        result = NEW zcl_qdrt_table_data_selector(
          name                = entity_name
          type                = entity_type
          metadata_provider   = metadata_provider
          aggregation_config  = aggregation_config
          sort_config         = sort_config
          output_field_config = output_fields_config
          filter_provider     = filter_provider ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
