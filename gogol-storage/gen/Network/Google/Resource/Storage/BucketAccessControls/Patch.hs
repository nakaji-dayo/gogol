{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Storage.BucketAccessControls.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an ACL entry on the specified bucket. This method supports patch
-- semantics.
--
-- /See:/ <https://developers.google.com/storage/docs/json_api/ Cloud Storage JSON API Reference> for @storage.bucketAccessControls.patch@.
module Network.Google.Resource.Storage.BucketAccessControls.Patch
    (
    -- * REST Resource
      BucketAccessControlsPatchResource

    -- * Creating a Request
    , bucketAccessControlsPatch
    , BucketAccessControlsPatch

    -- * Request Lenses
    , bacpBucket
    , bacpPayload
    , bacpUserProject
    , bacpEntity
    , bacpFields
    ) where

import Network.Google.Prelude
import Network.Google.Storage.Types

-- | A resource alias for @storage.bucketAccessControls.patch@ method which the
-- 'BucketAccessControlsPatch' request conforms to.
type BucketAccessControlsPatchResource =
     "storage" :>
       "v1" :>
         "b" :>
           Capture "bucket" Text :>
             "acl" :>
               Capture "entity" Text :>
                 QueryParam "userProject" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] BucketAccessControl :>
                         Patch '[JSON] BucketAccessControl

-- | Updates an ACL entry on the specified bucket. This method supports patch
-- semantics.
--
-- /See:/ 'bucketAccessControlsPatch' smart constructor.
data BucketAccessControlsPatch = BucketAccessControlsPatch'
    { _bacpBucket :: !Text
    , _bacpPayload :: !BucketAccessControl
    , _bacpUserProject :: !(Maybe Text)
    , _bacpEntity :: !Text
    , _bacpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BucketAccessControlsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bacpBucket'
--
-- * 'bacpPayload'
--
-- * 'bacpUserProject'
--
-- * 'bacpEntity'
--
-- * 'bacpFields'
bucketAccessControlsPatch
    :: Text -- ^ 'bacpBucket'
    -> BucketAccessControl -- ^ 'bacpPayload'
    -> Text -- ^ 'bacpEntity'
    -> BucketAccessControlsPatch
bucketAccessControlsPatch pBacpBucket_ pBacpPayload_ pBacpEntity_ = 
    BucketAccessControlsPatch'
    { _bacpBucket = pBacpBucket_
    , _bacpPayload = pBacpPayload_
    , _bacpUserProject = Nothing
    , _bacpEntity = pBacpEntity_
    , _bacpFields = Nothing
    }

-- | Name of a bucket.
bacpBucket :: Lens' BucketAccessControlsPatch Text
bacpBucket
  = lens _bacpBucket (\ s a -> s{_bacpBucket = a})

-- | Multipart request metadata.
bacpPayload :: Lens' BucketAccessControlsPatch BucketAccessControl
bacpPayload
  = lens _bacpPayload (\ s a -> s{_bacpPayload = a})

-- | The project to be billed for this request. Required for Requester Pays
-- buckets.
bacpUserProject :: Lens' BucketAccessControlsPatch (Maybe Text)
bacpUserProject
  = lens _bacpUserProject
      (\ s a -> s{_bacpUserProject = a})

-- | The entity holding the permission. Can be user-userId,
-- user-emailAddress, group-groupId, group-emailAddress, allUsers, or
-- allAuthenticatedUsers.
bacpEntity :: Lens' BucketAccessControlsPatch Text
bacpEntity
  = lens _bacpEntity (\ s a -> s{_bacpEntity = a})

-- | Selector specifying which fields to include in a partial response.
bacpFields :: Lens' BucketAccessControlsPatch (Maybe Text)
bacpFields
  = lens _bacpFields (\ s a -> s{_bacpFields = a})

instance GoogleRequest BucketAccessControlsPatch
         where
        type Rs BucketAccessControlsPatch =
             BucketAccessControl
        type Scopes BucketAccessControlsPatch =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/devstorage.full_control"]
        requestClient BucketAccessControlsPatch'{..}
          = go _bacpBucket _bacpEntity _bacpUserProject
              _bacpFields
              (Just AltJSON)
              _bacpPayload
              storageService
          where go
                  = buildClient
                      (Proxy :: Proxy BucketAccessControlsPatchResource)
                      mempty
