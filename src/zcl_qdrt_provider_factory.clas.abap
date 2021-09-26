"! <p class="shorttext synchronized" lang="en">Factory for creating providers</p>
CLASS zcl_qdrt_provider_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates filter converter</p>
      create_filter_converter
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_filter_converter,
      "! <p class="shorttext synchronized" lang="en">Creates entity metadata</p>
      create_entity_metadata
        IMPORTING
          entity_name   TYPE zif_qdrt_ty_global=>ty_entity_name
          entity_type   TYPE zif_qdrt_ty_global=>ty_entity_type
        RETURNING
          VALUE(result) TYPE REF TO zif_qdrt_entity_metadata_prov
        RAISING
          zcx_qdrt_appl_error,
      "! <p class="shorttext synchronized" lang="en">Creates metadata provider for VH at field level</p>
      create_field_vh_metadata_prov
        IMPORTING
          entity_name     TYPE zif_qdrt_ty_global=>ty_entity_name
          entity_type     TYPE zif_qdrt_ty_global=>ty_entity_type
          value_help_type TYPE string
          fieldname       TYPE fieldname
          field_type      TYPE zif_qdrt_ty_global=>ty_field_type
        RETURNING
          VALUE(result)   TYPE REF TO zif_qdrt_vh_metadata_provider
        RAISING
          zcx_qdrt_appl_error,
      "! <p class="shorttext synchronized" lang="en">Creates metadata provider for VH</p>
      create_vh_metadata_provider
        IMPORTING
          value_help_name TYPE shlpname
          value_help_type TYPE string
        RETURNING
          VALUE(result)   TYPE REF TO zif_qdrt_vh_metadata_provider
        RAISING
          zcx_qdrt_appl_error,
      "! <p class="shorttext synchronized" lang="en">Creates VH data provider</p>
      create_vh_data_provider
        IMPORTING
          vh_request type zif_qdrt_vh_data_provider=>ty_vh_request
        RETURNING
          VALUE(result)   TYPE REF TO zif_qdrt_vh_data_provider
        RAISING
          zcx_qdrt_appl_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_provider_factory IMPLEMENTATION.


  METHOD create_filter_converter.
    result = NEW zcl_qdrt_default_fc( ).
  ENDMETHOD.


  METHOD create_entity_metadata.
    result = SWITCH #( entity_type

      WHEN zif_qdrt_c_entity_types=>cds_view THEN
        NEW zcl_qdrt_cds_emp( entity_name )

      WHEN zif_qdrt_c_entity_types=>database_table OR
           zif_qdrt_c_entity_types=>view THEN
        NEW zcl_qdrt_table_emp(
          entity_type = entity_type
          entity_name = entity_name ) ).

    IF result IS NOT INITIAL.
      IF result->entity_exists( ) = abap_false.
        RAISE EXCEPTION TYPE zcx_qdrt_appl_error
          EXPORTING
            status = cl_rest_status_code=>gc_client_error_not_found
            text   = |Entity with type '{ entity_type }' and name '{ entity_name }' does not exist|.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          text   = |No metadata provider found for type '{ entity_type }'|.
    ENDIF.
  ENDMETHOD.


  METHOD create_field_vh_metadata_prov.
    result = SWITCH #( value_help_type

      WHEN zif_qdrt_c_value_help_type=>ddic_sh OR
           zif_qdrt_c_value_help_type=>check_table THEN
        NEW zcl_qdrt_ddic_shlp_for_ent_vmp(
          entity_name     = entity_name
          entity_type     = entity_type
          fieldname       = fieldname
          field_type      = field_type  ) ).

    IF result IS NOT INITIAL.
      IF result->entity_exists( ) = abap_false.
        RAISE EXCEPTION TYPE zcx_qdrt_appl_error
          EXPORTING
            status = cl_rest_status_code=>gc_client_error_not_found
            text   = |There is no value help for Entity Type: '{ entity_type }', Name: '{ entity_name }', Field: '{ fieldname }'|.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          text   = |No field VH metadata provider found for type '{ value_help_type }'|.
    ENDIF.
  ENDMETHOD.


  METHOD create_vh_metadata_provider.
    result = SWITCH #( value_help_type

      WHEN zif_qdrt_c_value_help_type=>ddic_sh OR
           zif_qdrt_c_value_help_type=>elementary_ddic_sh OR
           zif_qdrt_c_value_help_type=>collective_ddic_sh THEN
        NEW zcl_qdrt_ddic_shlp_vmp(
          name = value_help_name
          type = value_help_type ) ).

    IF result IS NOT INITIAL.
      IF result->entity_exists( ) = abap_false.
        RAISE EXCEPTION TYPE zcx_qdrt_appl_error
          EXPORTING
            status = cl_rest_status_code=>gc_client_error_not_found
            text   = |There is no value help with name '{ value_help_name }', Type: '{ value_help_type }'|.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          status = cl_rest_status_code=>gc_client_error_not_found
          text   = |No VH metadata provider found for type '{ value_help_type }'|.
    ENDIF.
  ENDMETHOD.


  METHOD create_vh_data_provider.
    result = SWITCH #( vh_request-type

      WHEN zif_qdrt_c_value_help_type=>fix_values THEN
        NEW zcl_qdrt_dom_fix_values_vhdp( rollname = vh_request-value_help_name )

      WHEN zif_qdrt_c_value_help_type=>elementary_ddic_sh THEN
        NEW zcl_qdrt_elementary_shlp_vhdp( value_help_name = vh_request-value_help_name )

      WHEN zif_qdrt_c_value_help_type=>check_table THEN
        NEW zcl_qdrt_checktable_vhdp(
          source_tab   = vh_request-source_tab
          source_field = vh_request-source_field
          checktable   = vh_request-value_help_name ) ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_qdrt_appl_error
        EXPORTING
          text = |Value Help Type { vh_request-type } is not supported|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
