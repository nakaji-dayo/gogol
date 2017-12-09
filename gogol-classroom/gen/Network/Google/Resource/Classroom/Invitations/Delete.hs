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
-- Module      : Network.Google.Resource.Classroom.Invitations.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an invitation. This method returns the following error codes: *
-- \`PERMISSION_DENIED\` if the requesting user is not permitted to delete
-- the requested invitation or for access errors. * \`NOT_FOUND\` if no
-- invitation exists with the requested ID.
--
-- /See:/ <https://developers.google.com/classroom/ Google Classroom API Reference> for @classroom.invitations.delete@.
module Network.Google.Resource.Classroom.Invitations.Delete
    (
    -- * REST Resource
      InvitationsDeleteResource

    -- * Creating a Request
    , invitationsDelete
    , InvitationsDelete

    -- * Request Lenses
    , idXgafv
    , idUploadProtocol
    , idPp
    , idAccessToken
    , idUploadType
    , idBearerToken
    , idId
    , idFields
    , idCallback
    ) where

import Network.Google.Classroom.Types
import Network.Google.Prelude

-- | A resource alias for @classroom.invitations.delete@ method which the
-- 'InvitationsDelete' request conforms to.
type InvitationsDeleteResource =
     "v1" :>
       "invitations" :>
         Capture "id" Text :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Deletes an invitation. This method returns the following error codes: *
-- \`PERMISSION_DENIED\` if the requesting user is not permitted to delete
-- the requested invitation or for access errors. * \`NOT_FOUND\` if no
-- invitation exists with the requested ID.
--
-- /See:/ 'invitationsDelete' smart constructor.
data InvitationsDelete = InvitationsDelete'
    { _idXgafv :: !(Maybe Xgafv)
    , _idUploadProtocol :: !(Maybe Text)
    , _idPp :: !Bool
    , _idAccessToken :: !(Maybe Text)
    , _idUploadType :: !(Maybe Text)
    , _idBearerToken :: !(Maybe Text)
    , _idId :: !Text
    , _idFields :: !(Maybe Text)
    , _idCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InvitationsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idXgafv'
--
-- * 'idUploadProtocol'
--
-- * 'idPp'
--
-- * 'idAccessToken'
--
-- * 'idUploadType'
--
-- * 'idBearerToken'
--
-- * 'idId'
--
-- * 'idFields'
--
-- * 'idCallback'
invitationsDelete
    :: Text -- ^ 'idId'
    -> InvitationsDelete
invitationsDelete pIdId_ = 
    InvitationsDelete'
    { _idXgafv = Nothing
    , _idUploadProtocol = Nothing
    , _idPp = True
    , _idAccessToken = Nothing
    , _idUploadType = Nothing
    , _idBearerToken = Nothing
    , _idId = pIdId_
    , _idFields = Nothing
    , _idCallback = Nothing
    }

-- | V1 error format.
idXgafv :: Lens' InvitationsDelete (Maybe Xgafv)
idXgafv = lens _idXgafv (\ s a -> s{_idXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
idUploadProtocol :: Lens' InvitationsDelete (Maybe Text)
idUploadProtocol
  = lens _idUploadProtocol
      (\ s a -> s{_idUploadProtocol = a})

-- | Pretty-print response.
idPp :: Lens' InvitationsDelete Bool
idPp = lens _idPp (\ s a -> s{_idPp = a})

-- | OAuth access token.
idAccessToken :: Lens' InvitationsDelete (Maybe Text)
idAccessToken
  = lens _idAccessToken
      (\ s a -> s{_idAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
idUploadType :: Lens' InvitationsDelete (Maybe Text)
idUploadType
  = lens _idUploadType (\ s a -> s{_idUploadType = a})

-- | OAuth bearer token.
idBearerToken :: Lens' InvitationsDelete (Maybe Text)
idBearerToken
  = lens _idBearerToken
      (\ s a -> s{_idBearerToken = a})

-- | Identifier of the invitation to delete.
idId :: Lens' InvitationsDelete Text
idId = lens _idId (\ s a -> s{_idId = a})

-- | Selector specifying which fields to include in a partial response.
idFields :: Lens' InvitationsDelete (Maybe Text)
idFields = lens _idFields (\ s a -> s{_idFields = a})

-- | JSONP
idCallback :: Lens' InvitationsDelete (Maybe Text)
idCallback
  = lens _idCallback (\ s a -> s{_idCallback = a})

instance GoogleRequest InvitationsDelete where
        type Rs InvitationsDelete = Empty
        type Scopes InvitationsDelete =
             '["https://www.googleapis.com/auth/classroom.rosters"]
        requestClient InvitationsDelete'{..}
          = go _idId _idXgafv _idUploadProtocol (Just _idPp)
              _idAccessToken
              _idUploadType
              _idBearerToken
              _idCallback
              _idFields
              (Just AltJSON)
              classroomService
          where go
                  = buildClient
                      (Proxy :: Proxy InvitationsDeleteResource)
                      mempty
