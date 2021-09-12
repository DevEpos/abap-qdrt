"! <p class="shorttext synchronized" lang="en">Metadata of Table/View</p>
CLASS zcl_qdrt_table_emp DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_entity_metadata_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_qdrt_entity_metadata_prov~get_field_config REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_metadata REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_field_metadata REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_fields_metadata REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      read_metadata REDEFINITION.
  PRIVATE SECTION.
    DATA:
      fields TYPE STANDARD TABLE OF dfies WITH EMPTY KEY,
      BEGIN OF metadata,
        fields TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      END OF metadata.
ENDCLASS.



CLASS zcl_qdrt_table_emp IMPLEMENTATION.


  METHOD zif_qdrt_entity_metadata_prov~get_metadata.
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_config.

  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_metadata.
    CHECK metadata IS NOT INITIAL.

    IF type = zif_qdrt_c_global=>c_field_types-normal_field.
      result = VALUE #( metadata-fields[ name = to_lower( fieldname ) ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD read_metadata.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = entity_name
      TABLES
        dfies_tab      = fields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
      metadata-fields = VALUE #( BASE metadata-fields
        ( to_field_metadata( field_info = CORRESPONDING zif_qdrt_ty_global=>ty_field_info(
            <field> MAPPING name               = fieldname
                            is_key             = keyflag
                            length             = leng
                            has_fix_values     = valexi
                            short_description  = scrtext_s
                            medium_description = scrtext_m
                            long_description   = scrtext_l
                            field_text         = fieldtext
                            has_value_help     = f4availabl
                            ref_field          = reffield
                            ref_table          = reftable
                            is_lowercase       = lowercase ) ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_fields_metadata.
    result = metadata-fields.
  ENDMETHOD.

ENDCLASS.
