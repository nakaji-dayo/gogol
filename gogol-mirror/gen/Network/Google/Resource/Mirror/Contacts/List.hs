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
-- Module      : Network.Google.Resource.Mirror.Contacts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of contacts for the authenticated user.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.contacts.list@.
module Network.Google.Resource.Mirror.Contacts.List
    (
    -- * REST Resource
      ContactsListResource

    -- * Creating a Request
    , contactsList
    , ContactsList

    -- * Request Lenses
    , clFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.contacts.list@ method which the
-- 'ContactsList' request conforms to.
type ContactsListResource =
     "mirror" :>
       "v1" :>
         "contacts" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] ContactsListResponse

-- | Retrieves a list of contacts for the authenticated user.
--
-- /See:/ 'contactsList' smart constructor.
newtype ContactsList = ContactsList'
    { _clFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clFields'
contactsList
    :: ContactsList
contactsList = 
    ContactsList'
    { _clFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
clFields :: Lens' ContactsList (Maybe Text)
clFields = lens _clFields (\ s a -> s{_clFields = a})

instance GoogleRequest ContactsList where
        type Rs ContactsList = ContactsListResponse
        type Scopes ContactsList =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient ContactsList'{..}
          = go _clFields (Just AltJSON) mirrorService
          where go
                  = buildClient (Proxy :: Proxy ContactsListResource)
                      mempty
