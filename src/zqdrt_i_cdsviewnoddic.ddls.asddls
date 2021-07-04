@AbapCatalog.sqlViewName: 'ZQDRTICDVND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Views without DDIC SQL View'

define view ZQDRT_I_CdsViewNoDdic
  as select from    ddddlsrc     as DDLSource
    inner join      dd02b        as StructuredObject on  DDLSource.ddlname  = StructuredObject.strucobjn
                                                     and DDLSource.as4local = 'A'
    left outer join ddddlsrc02bt as Text             on  Text.ddlname    = DDLSource.ddlname
                                                     and Text.strucobjn  = StructuredObject.strucobjn
                                                     and Text.as4local   = 'A'
                                                     and Text.ddlanguage = $session.system_language
    inner join      tadir        as Repo             on  DDLSource.ddlname = Repo.obj_name
                                                     and Repo.pgmid        = 'R3TR'
                                                     and object            = 'DDLS'
{
  key StructuredObject.strucobjn     as EntityId,
      StructuredObject.strucobjn_raw as RawEntityId,
      Text.ddtext                    as Description,
      Repo.devclass                  as DevelopmentPackage,
      Repo.author                    as CreatedBy,
      Repo.created_on                as CreatedDate,
      DDLSource.as4user              as ChangedBy,
      DDLSource.as4date              as ChangedDate,
      DDLSource.source_type          as SourceType,
      'C'                            as Type
}

where
       StructuredObject.as4local = 'A'
  and(
       DDLSource.source_type     = 'F' // Table Function
    or DDLSource.source_type     = 'H' // Hierarchy view
    or DDLSource.source_type     = 'P' // Projection view
    or DDLSource.source_type     = 'W' // CDS View entity
  )
