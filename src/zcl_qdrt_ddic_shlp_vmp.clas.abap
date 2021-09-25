"! <p class="shorttext synchronized" lang="en">Metadata provider for DDIC value help</p>
CLASS zcl_qdrt_ddic_shlp_vmp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_vh_metadata_provider.
    METHODS:
      constructor
        IMPORTING
          name TYPE shlpname
          type TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_metadata.
             INCLUDE TYPE zif_qdrt_ty_global=>ty_vh_metadata.
    TYPES: END OF ty_metadata.
    DATA:
      exists          TYPE abap_bool VALUE abap_undefined,
      metadata        TYPE ty_metadata,
      value_help_type TYPE string,
      value_help_name TYPE shlpname.

    METHODS: read_metadata.
ENDCLASS.



CLASS zcl_qdrt_ddic_shlp_vmp IMPLEMENTATION.

  METHOD constructor.
    me->value_help_name = name.
    me->value_help_type = type.
    read_metadata( ).
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~entity_exists.
    IF exists = abap_undefined.
      SELECT SINGLE @abap_true
        FROM dd30l
        WHERE shlpname = @value_help_name
        INTO @exists.
      IF sy-subrc <> 0.
        exists = abap_false.
      ENDIF.
    ENDIF.

    result = exists.
  ENDMETHOD.


  METHOD zif_qdrt_metadata_provider~get_metadata.
    result = REF #( metadata ).
  ENDMETHOD.


  METHOD read_metadata.
    DATA:
      shlp_descriptor TYPE shlp_descr.
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = value_help_name
        shlptype = 'SH'
      IMPORTING
        shlp     = shlp_descriptor.


    metadata = CORRESPONDING #( zcl_qdrt_metadata_util=>convert_to_vh_metadata( shlp_descr = shlp_descriptor ) ).

    IF shlp_descriptor IS NOT INITIAL AND
        shlp_descriptor-shlpname IS NOT INITIAL.
      DATA(shlp_description) = zcl_qdrt_text_util=>get_short_text(
        object_type = zif_qdrt_c_global=>c_tadir_types-search_help
        object_name = CONV #( value_help_name ) ).
      metadata-description = shlp_description.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
