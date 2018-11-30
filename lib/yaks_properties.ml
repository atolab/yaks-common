module Access = struct               
  module Key = struct 
    let key = "is.yaks.access" 
    let id = "is.yaks.access.id" 
    let alias = "is.yaks.access.alias" 
    let cache_size = "is.yaks.access.cachesize"
    let encoding = "is.yaks.access.encoding"
    let subscription_id = "is.yaks.subscription.id"
  end
end

module Storage = struct
  module Key = struct 
    let key = "is.yaks.storage"
    let id = "is.yaks.storage.id"    
    let alias = "is.yaks.storage.alias"    
    let config = "is.yaks.storage.config" 
    let set = "is.yaks.storage.set"
  end
end