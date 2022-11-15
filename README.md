# purescript-ix-maps

A Map data structure that derives keys from values

## Using a conventional Map

Assume you have the following data structure:

```hs
newtype Id = Id String

newtype User = User { id :: Id, name :: String }
```

And you're about to create a `Map` that uses the `Id` as key. Oftentimes it's desired that the following should not be possible:

```hs
myMap :: Map Id User
myMap = Map.empty
  # Map.insert (Id "foo") (User { id: "bar", name: "Santa" })
```

As you can see, there is no guarantee that the `Id` inside the `User` type (`Id "bar"`) equals the `Id` that was used as key (`Id "foo"`).


## Using IxMap

This library provides a wrapper around [Data.Map#Map](https://pursuit.purescript.org/packages/purescript-ordered-collections/3.0.0/docs/Data.Map#t:Map)

You'd use it as follows:

1. First you have to define a way to receive a key from your value type. You do this by providing an instance for the `Indexed` type class:

   ```hs
   instance Indexed Id User where
     getIndex (User { id }) = id
   ```

2. Then you can use the `IxMap` API for safe CRUD operations. For instance with the `insert` function you can insert an entry to the map by only providing the value:

   ```hs
   myMap :: IxMap Id User
   myMap = IxMap.empty
     # IxMap.insert (User { id: Id "bar", name: "Santa" })
   ```

