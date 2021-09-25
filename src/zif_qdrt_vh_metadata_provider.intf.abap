"! <p class="shorttext synchronized" lang="en">Metdata provider for value helps</p>
INTERFACE zif_qdrt_vh_metadata_provider
  PUBLIC.
  INTERFACES zif_qdrt_metadata_provider.

  ALIASES:
    get_metadata FOR zif_qdrt_metadata_provider~get_metadata,
    entity_exists FOR zif_qdrt_metadata_provider~entity_exists.
ENDINTERFACE.
