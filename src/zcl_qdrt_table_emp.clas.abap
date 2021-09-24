"! <p class="shorttext synchronized" lang="en">Metadata of Table/View</p>
CLASS zcl_qdrt_table_emp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_entity_metadata_prov.
    METHODS:
      constructor
        IMPORTING
          entity_name TYPE tabname
          entity_type TYPE zif_qdrt_ty_global=>ty_entity_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      fields      TYPE STANDARD TABLE OF dfies WITH EMPTY KEY,
      entity_type TYPE zif_qdrt_ty_global=>ty_entity_type,
      entity_name TYPE tabname,
      exists      TYPE abap_bool VALUE abap_undefined,
      BEGIN OF metadata,
        BEGIN OF entity,
          name        TYPE string,
          description TYPE string,
        END OF entity,
        fields TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      END OF metadata.

    METHODS:
      read_metadata.
ENDCLASS.



CLASS zcl_qdrt_table_emp IMPLEMENTATION.


  METHOD constructor.
    me->entity_name = entity_name.
    me->entity_type = entity_type.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~entity_exists.
    IF exists = abap_undefined.
      IF entity_type = zif_qdrt_c_entity_types=>database_table.
        SELECT SINGLE @abap_true
          FROM zqdrt_i_dbtable
          WHERE tablename = @entity_name
          INTO @exists.
      ELSEIF entity_type = zif_qdrt_c_entity_types=>view.
        SELECT SINGLE @abap_true
          FROM zqdrt_i_dbview
          WHERE viewname = @entity_name
          INTO @exists.
      ENDIF.
      IF sy-subrc <> 0.
        exists = abap_false.
      ENDIF.
    ENDIF.
    result = exists.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~get_metadata.
    read_metadata( ).
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_config.
    RETURN.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_metadata.
    read_metadata( ).

    IF type = zif_qdrt_c_global=>c_field_types-normal_field.
      result = VALUE #( metadata-fields[ name = to_lower( fieldname ) ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD read_metadata.
    CHECK:
      zif_qdrt_entity_metadata_prov~entity_exists( ),
      metadata IS INITIAL.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = entity_name
      TABLES
        dfies_tab      = fields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    metadata-entity = VALUE #(
      name        = entity_name
      description = zcl_qdrt_text_util=>get_short_text(
        object_type = COND #( WHEN entity_type = zif_qdrt_c_entity_types=>database_table THEN 'TABL' ELSE 'VIEW' )
        object_name = CONV #( entity_name ) ) ).

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>) WHERE datatype <> 'CLNT'.
      metadata-fields = VALUE #( BASE metadata-fields
        ( zcl_qdrt_metadata_util=>convert_to_field_metadata(
            entity_name = entity_name
            field_info  = CORRESPONDING zif_qdrt_ty_global=>ty_field_info(
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
    read_metadata( ).
    result = metadata-fields.
  ENDMETHOD.

ENDCLASS.
