{-# LANGUAGE OverloadedStrings #-}
module TDF.Trials.Seed where

import           Database.Persist       (Entity(..), (=.), upsert)
import           Database.Persist.Sql   (SqlPersistT)

import           TDF.Trials.Models

seedTrials :: SqlPersistT IO ()
seedTrials = do
  -- Seed a handful of subjects that Trials screens can rely on.
  let subjects = ["Producción","Guitar","Bass","Drums","Voice","Ableton","Modular","DJ"]
  subjectEntities <- mapM (\name -> upsert (Subject name True) [SubjectActive =. True]) subjects

  -- Provide two simple packages per subject so scheduling flows have data.
  let mkPkg sid label hrs price = PackageCatalog sid label hrs price 120 "credit_only" True
  mapM_ (\(Entity sid _) -> do
           _ <- upsert (mkPkg sid "12h" 12 30000) [PackageCatalogPriceCents =. 30000]
           _ <- upsert (mkPkg sid "24h" 24 50000) [PackageCatalogPriceCents =. 50000]
           pure ()
        ) subjectEntities

  pure ()
