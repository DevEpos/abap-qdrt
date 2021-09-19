"! <p class="shorttext synchronized" lang="en">Fields config base</p>
CLASS zcl_qdrt_entity_metadata_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES zif_qdrt_entity_metadata_prov
      ABSTRACT METHODS
      entity_exists
      get_field_config
      get_metadata
      get_field_metadata
      get_fields_metadata.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Initializes the metadata</p>
      init.
  PROTECTED SECTION.
    CONSTANTS:
      c_boolean_values TYPE string VALUE ' X-'.

    DATA:
      entity_type TYPE zif_qdrt_ty_global=>ty_entity_type,
      entity_name TYPE zif_qdrt_ty_global=>ty_entity_name,
      exists      TYPE abap_bool VALUE abap_undefined.

    METHODS:
      constructor
        IMPORTING
          entity_name TYPE zif_qdrt_ty_global=>ty_entity_name
          entity_type TYPE zif_qdrt_ty_global=>ty_entity_type,
      read_metadata ABSTRACT,

      to_field_metadata
        IMPORTING
          VALUE(field_info) TYPE zif_qdrt_ty_global=>ty_field_info
        RETURNING
          VALUE(result)     TYPE zif_qdrt_ty_global=>ty_field_metadata,

      is_boolean_type
        IMPORTING
          rollname      TYPE rollname
        RETURNING
          VALUE(result) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Retrieves DFIES info of data element</p>
      get_dtel_info
        IMPORTING
          rollname      TYPE rollname
        RETURNING
          VALUE(result) TYPE dfies,
      "! <p class="shorttext synchronized" lang="en">Retrieves object short text</p>
      get_short_text
        IMPORTING
          object_type   TYPE trobjtype
          object_name   TYPE sobj_name
        RETURNING
          VALUE(result) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_entity_metadata_base IMPLEMENTATION.


  METHOD constructor.
    me->entity_name = entity_name.
    me->entity_type = entity_type.
  ENDMETHOD.


  METHOD init.
    read_metadata( ).
  ENDMETHOD.


  METHOD to_field_metadata.
    DATA:
      fields_with_dfvh TYPE string_table.

    result = CORRESPONDING #( field_info ).
    TRANSLATE result-name TO LOWER CASE.
    result-scale = field_info-decimals.

    result-type = SWITCH #( field_info-datatype
      " Character Types
      WHEN 'CHAR' OR
           'LANG' OR
           'CLNT' OR
           'LCHR' OR
           'SSTR' OR
           'STRG' OR
           'GGM1' OR " Geo Data, take as string type for now
           'CUKY' OR
           'NUMC' OR " Normally NUMC is not to be considered as Number
           'UNIT' THEN zif_qdrt_c_edm_types=>string
      " Float types
      WHEN 'D16D' OR
           'D16N' OR
           'D16R' OR
           'D16S' OR
           'D34D' OR
           'D34N' OR
           'D34R' OR
           'D34S' THEN zif_qdrt_c_edm_types=>float
      " Date Types
      WHEN 'DATN' OR
           'ACCP' OR " Posting Period YYYYMM
           'DATS' THEN zif_qdrt_c_edm_types=>date
      " Decimal types
      WHEN 'DEC' OR
           'CURR' OR
           'QUAN' THEN zif_qdrt_c_edm_types=>decimal
      " Double types
      WHEN 'FLTP' THEN zif_qdrt_c_edm_types=>double
      " Integer types
      WHEN 'INT1' THEN zif_qdrt_c_edm_types=>byte
      WHEN 'INT2' THEN zif_qdrt_c_edm_types=>int16
      WHEN 'INT4' THEN zif_qdrt_c_edm_types=>int32
      WHEN 'INT8' THEN zif_qdrt_c_edm_types=>int64
      " Binary types
      WHEN 'LRAW' OR
           'RSTR' THEN zif_qdrt_c_edm_types=>binary
      " Byte types
      WHEN 'RAW' THEN zif_qdrt_c_edm_types=>byte
      " Time
      WHEN 'TIMN' OR
           'TIMS' THEN zif_qdrt_c_edm_types=>time
      " Time stamp with Data/Time
      WHEN 'UTCL' THEN zif_qdrt_c_edm_types=>date_time ).

    " check if timestamp
    IF field_info-domname = 'TZNTSTMPL'.
      result-type = zif_qdrt_c_edm_types=>date_time_offset.
    ENDIF.

    IF field_info-domname = 'TZNTSTMPS'.
      result-type = zif_qdrt_c_edm_types=>date_time.
    ENDIF.

    IF field_info-domname = 'SYSUUID'.
      " Note: /ui2/cl_json does not appear to be able to serialize guids
      result-type = zif_qdrt_c_edm_types=>guid.
    ENDIF.

    IF result-type CP 'Int*' OR
        result-type = zif_qdrt_c_edm_types=>byte OR
        result-type = zif_qdrt_c_edm_types=>decimal OR
        result-type = zif_qdrt_c_edm_types=>float.
      result-is_numeric = abap_true.
    ENDIF.

    IF result-type CP 'Int*' OR
        result-type = zif_qdrt_c_edm_types=>decimal OR
        result-type = zif_qdrt_c_edm_types=>float.
      result-is_total_possible = abap_true.
    ENDIF.

    IF result-is_numeric = abap_true OR field_info-decimals > 0.
      result-precision = field_info-length.
      result-scale = field_info-decimals.
    ELSE.
      IF field_info-datatype = 'LANG'.
        result-max_length = 2.
      ELSE.
        result-max_length = field_info-length.
      ENDIF.
    ENDIF.

    IF field_info-datatype = 'UNIT'.
      result-semantics = 'unit-of-measure'.
    ELSEIF field_info-datatype = 'CUKY'.
      result-semantics = 'currency-code'.
    ENDIF.

    IF field_info-domname = 'BOOLE_D' OR
        field_info-domname = 'BOOLEAN' OR
        field_info-domname = 'XFELD'.
      result-type = zif_qdrt_c_edm_types=>boolean.
    ELSEIF field_info-datatype = 'CHAR' AND
        field_info-length = 1 AND
        field_info-domname IS NOT INITIAL.
      IF is_boolean_type( field_info-rollname ).
        result-type = zif_qdrt_c_edm_types=>boolean.
        " Set rollname explicitly to XFELD so JSON serializer detect's it as boolean type
        result-rollname = 'XFELD'.
      ENDIF.
    ENDIF.

    IF result-type = zif_qdrt_c_edm_types=>boolean.
      CLEAR: field_info-has_fix_values,
             result-has_value_help.
    ENDIF.

    IF field_info-ref_field IS NOT INITIAL AND
        field_info-ref_table IS NOT INITIAL AND
        field_info-ref_table = entity_name.

      result-unit_field = to_lower( field_info-ref_field ).
    ENDIF.

    " handle value help type
    IF field_info-checktable IS NOT INITIAL.
      result-value_help_type = zif_qdrt_c_value_help_type=>check_table.
    ELSEIF field_info-has_fix_values = abap_true.
      result-value_help_type = zif_qdrt_c_value_help_type=>fix_values.
      fields_with_dfvh = VALUE #( BASE fields_with_dfvh ( result-name ) ).
    ELSEIF result-has_value_help = abap_true.
      IF result-type = zif_qdrt_c_edm_types=>date.
        result-value_help_type = zif_qdrt_c_value_help_type=>date.
      ELSE.
        result-value_help_type = zif_qdrt_c_value_help_type=>elementary_ddic_sh.
      ENDIF.
    ENDIF.

    " handle display format
    IF result-type = zif_qdrt_c_edm_types=>string AND field_info-is_lowercase = abap_false.
      result-display_format = 'UpperCase'.
    ENDIF.
  ENDMETHOD.


  METHOD is_boolean_type.
    DATA(dtel_doma_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( rollname ) ).
    DATA(fixed_values) = dtel_doma_descr->get_ddic_fixed_values( ).
    IF lines( fixed_values ) <= 3.

      LOOP AT fixed_values ASSIGNING FIELD-SYMBOL(<fixed_value>).
        IF <fixed_value>-high IS NOT INITIAL.
          RETURN.
        ENDIF.

        IF c_boolean_values NS <fixed_value>-low.
          RETURN.
        ENDIF.
      ENDLOOP.

      result = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_dtel_info.
    CHECK rollname IS NOT INITIAL.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = rollname
        all_types      = abap_true
      IMPORTING
        dfies_wa       = result
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
  ENDMETHOD.


  METHOD get_short_text.
    DATA:
      texts TYPE TABLE OF seu_objtxt.

    texts = VALUE #( ( object = object_type obj_name = object_name ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.

    result = texts[ 1 ]-stext.
  ENDMETHOD.

ENDCLASS.
