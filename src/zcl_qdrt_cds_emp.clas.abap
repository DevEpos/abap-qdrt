"! <p class="shorttext synchronized" lang="en">Metadata of CDS view</p>
CLASS zcl_qdrt_cds_emp DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_entity_metadata_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_qdrt_entity_metadata_prov~entity_exists REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_field_config REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_metadata REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_field_metadata REDEFINITION,
      zif_qdrt_entity_metadata_prov~get_fields_metadata REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      read_metadata REDEFINITION.
  PRIVATE SECTION.
    DATA:
      header_info        TYPE dd02bv,
      ddl_view_name      TYPE viewname,
      fields_cds         TYPE dd03ndvtab,
      fields             TYPE STANDARD TABLE OF dfies WITH EMPTY KEY,
      association_header TYPE dd08bvtab,
      association_fields TYPE dd05bvtab,
      parameters         TYPE dd10bvtab,

      BEGIN OF metadata,
        fields     TYPE zif_qdrt_ty_global=>ty_fields_metadata,
        parameters TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      END OF metadata.

    METHODS:
      fill_fields_metadata
        IMPORTING
          fields_cds TYPE dd03ndvtab,
      fill_param_metadata
        IMPORTING
          params TYPE dd10bvtab.
ENDCLASS.



CLASS zcl_qdrt_cds_emp IMPLEMENTATION.


  METHOD zif_qdrt_entity_metadata_prov~entity_exists.

    IF exists = abap_undefined.
      IF sy-saprl >= 751.
        DATA(source_type_where_cond) = `source_type IN @<valid_source_types>`.
        ASSIGN zcl_qdrt_cds_util=>valid_ddl_source_types TO FIELD-SYMBOL(<valid_source_types>).

        SELECT SINGLE @abap_true
          FROM ddddlsrc AS ddl
            INNER JOIN zqdrt_i_cdsview AS cds
              ON ddl~ddlname = cds~AltEntityId
          WHERE entityid = @entity_name
            AND (source_type_where_cond)
          INTO @exists.
      ELSE.
        SELECT SINGLE @abap_true
          FROM zqdrt_i_cdsview
          WHERE entityid = @entity_name
          INTO @exists.
      ENDIF.
      IF sy-subrc <> 0.
        exists = abap_false.
      ENDIF.
    ENDIF.
    result = exists.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_metadata.
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_config.
    RETURN.
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_field_metadata.
    CHECK metadata IS NOT INITIAL.

    IF type = zif_qdrt_c_global=>c_field_types-normal_field.
      result = VALUE #( metadata-fields[ name = to_lower( fieldname ) ] OPTIONAL ).
    ELSEIF type = zif_qdrt_c_global=>c_field_types-parameter.
      result = VALUE #( metadata-parameters[ name = to_lower( fieldname ) ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD read_metadata.
    DATA(dd_sobject) = cl_dd_sobject_factory=>create( ).

    TRY.
        dd_sobject->read(
          EXPORTING
            get_state    = 'A'
            withtext     = abap_true
            sobjnames    = VALUE #( ( entity_name ) )
          IMPORTING
            dd02bv_tab   = DATA(headers)
            dd02bndv_tab = DATA(nodes)
            dd03ndv_tab  = DATA(fields_cds)
            dd08bv_tab   = association_header
            dd05bv_tab   = association_fields
            dd10bv_tab   = parameters ).

        IF headers IS INITIAL.
          RETURN.
        ENDIF.

      CATCH cx_dd_sobject_get.
        RETURN.
    ENDTRY.

    header_info = headers[ 1 ].
    ddl_view_name = nodes[ 1 ]-dbtabname.

    fill_fields_metadata( fields_cds ).
    fill_param_metadata( parameters ).
  ENDMETHOD.


  METHOD zif_qdrt_entity_metadata_prov~get_fields_metadata.
    result = metadata-fields.
  ENDMETHOD.


  METHOD fill_fields_metadata.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = entity_name
      TABLES
        dfies_tab      = fields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    LOOP AT fields_cds ASSIGNING FIELD-SYMBOL(<field_cds>).
      ASSIGN fields[ fieldname = <field_cds>-fieldname ] TO FIELD-SYMBOL(<field_dd>).
      metadata-fields = VALUE #( BASE metadata-fields
        ( to_field_metadata( field_info = CORRESPONDING zif_qdrt_ty_global=>ty_field_info(
            <field_dd> MAPPING name               = fieldname
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


  METHOD fill_param_metadata.
    DATA:
      type_info TYPE dfies.

    CHECK params IS NOT INITIAL.

    LOOP AT params ASSIGNING FIELD-SYMBOL(<param>).
      CLEAR type_info.
      DATA(param_info) = VALUE zif_qdrt_ty_global=>ty_field_info(
        name               = <param>-parametername
        datatype           = <param>-datatype
        decimals           = <param>-decimals
        length             = <param>-leng
        rollname           = <param>-rollname
        field_text         = <param>-ddtext ).
      IF <param>-rollname IS NOT INITIAL.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = entity_name
          IMPORTING
            dfies_wa       = type_info
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        param_info-domname            = type_info-domname.
        param_info-has_fix_values     = type_info-valexi.
        param_info-short_description  = type_info-scrtext_s.
        param_info-medium_description = type_info-scrtext_m.
        param_info-long_description   = type_info-scrtext_l.
        param_info-has_value_help     = type_info-f4availabl.
        param_info-ref_field          = type_info-reffield.
        param_info-ref_table          = type_info-reftable.
        param_info-checktable         = type_info-checktable.
        param_info-is_lowercase       = type_info-lowercase.

      ENDIF.

      metadata-parameters = VALUE #( BASE metadata-parameters ( to_field_metadata( param_info ) ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
