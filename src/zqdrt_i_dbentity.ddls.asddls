@AbapCatalog.sqlViewName: 'ZQDRTIDBENT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database entity (view,table,cds)'

define view ZQDRT_I_DbEntity
  as select from ZQDRT_I_DbTable
{
  key TableName as EntityId,
      TableName as RawEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
union select from ZQDRT_I_DbView
{
  key ViewName as EntityId,
      ViewName as RawEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
union select from ZQDRT_I_CdsView
{
  key EntityId,
      RawEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
union select from ZQDRT_I_CdsViewNoDdic
{
  key EntityId,
      RawEntityId,
      Description,
      DevelopmentPackage,
      CreatedBy,
      CreatedDate,
      ChangedBy,
      ChangedDate,
      Type
}
