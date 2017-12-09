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
-- Module      : Network.Google.Resource.DFAReporting.EventTags.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing event tag. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.eventTags.patch@.
module Network.Google.Resource.DFAReporting.EventTags.Patch
    (
    -- * REST Resource
      EventTagsPatchResource

    -- * Creating a Request
    , eventTagsPatch
    , EventTagsPatch

    -- * Request Lenses
    , etpProFileId
    , etpPayload
    , etpId
    , etpFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.eventTags.patch@ method which the
-- 'EventTagsPatch' request conforms to.
type EventTagsPatchResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "eventTags" :>
               QueryParam "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] EventTag :> Patch '[JSON] EventTag

-- | Updates an existing event tag. This method supports patch semantics.
--
-- /See:/ 'eventTagsPatch' smart constructor.
data EventTagsPatch = EventTagsPatch'
    { _etpProFileId :: !(Textual Int64)
    , _etpPayload :: !EventTag
    , _etpId :: !(Textual Int64)
    , _etpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventTagsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etpProFileId'
--
-- * 'etpPayload'
--
-- * 'etpId'
--
-- * 'etpFields'
eventTagsPatch
    :: Int64 -- ^ 'etpProFileId'
    -> EventTag -- ^ 'etpPayload'
    -> Int64 -- ^ 'etpId'
    -> EventTagsPatch
eventTagsPatch pEtpProFileId_ pEtpPayload_ pEtpId_ = 
    EventTagsPatch'
    { _etpProFileId = _Coerce # pEtpProFileId_
    , _etpPayload = pEtpPayload_
    , _etpId = _Coerce # pEtpId_
    , _etpFields = Nothing
    }

-- | User profile ID associated with this request.
etpProFileId :: Lens' EventTagsPatch Int64
etpProFileId
  = lens _etpProFileId (\ s a -> s{_etpProFileId = a})
      . _Coerce

-- | Multipart request metadata.
etpPayload :: Lens' EventTagsPatch EventTag
etpPayload
  = lens _etpPayload (\ s a -> s{_etpPayload = a})

-- | Event tag ID.
etpId :: Lens' EventTagsPatch Int64
etpId
  = lens _etpId (\ s a -> s{_etpId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
etpFields :: Lens' EventTagsPatch (Maybe Text)
etpFields
  = lens _etpFields (\ s a -> s{_etpFields = a})

instance GoogleRequest EventTagsPatch where
        type Rs EventTagsPatch = EventTag
        type Scopes EventTagsPatch =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient EventTagsPatch'{..}
          = go _etpProFileId (Just _etpId) _etpFields
              (Just AltJSON)
              _etpPayload
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy EventTagsPatchResource)
                      mempty
