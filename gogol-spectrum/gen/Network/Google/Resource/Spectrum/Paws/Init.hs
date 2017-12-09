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
-- Module      : Network.Google.Resource.Spectrum.Paws.Init
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes the connection between a white space device and the
-- database.
--
-- /See:/ <http://developers.google.com/spectrum Google Spectrum Database API Reference> for @spectrum.paws.init@.
module Network.Google.Resource.Spectrum.Paws.Init
    (
    -- * REST Resource
      PawsInitResource

    -- * Creating a Request
    , pawsInit
    , PawsInit

    -- * Request Lenses
    , piPayload
    , piFields
    ) where

import Network.Google.Prelude
import Network.Google.Spectrum.Types

-- | A resource alias for @spectrum.paws.init@ method which the
-- 'PawsInit' request conforms to.
type PawsInitResource =
     "spectrum" :>
       "v1explorer" :>
         "paws" :>
           "init" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] PawsInitRequest :>
                   Post '[JSON] PawsInitResponse

-- | Initializes the connection between a white space device and the
-- database.
--
-- /See:/ 'pawsInit' smart constructor.
data PawsInit = PawsInit'
    { _piPayload :: !PawsInitRequest
    , _piFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PawsInit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piPayload'
--
-- * 'piFields'
pawsInit
    :: PawsInitRequest -- ^ 'piPayload'
    -> PawsInit
pawsInit pPiPayload_ = 
    PawsInit'
    { _piPayload = pPiPayload_
    , _piFields = Nothing
    }

-- | Multipart request metadata.
piPayload :: Lens' PawsInit PawsInitRequest
piPayload
  = lens _piPayload (\ s a -> s{_piPayload = a})

-- | Selector specifying which fields to include in a partial response.
piFields :: Lens' PawsInit (Maybe Text)
piFields = lens _piFields (\ s a -> s{_piFields = a})

instance GoogleRequest PawsInit where
        type Rs PawsInit = PawsInitResponse
        type Scopes PawsInit = '[]
        requestClient PawsInit'{..}
          = go _piFields (Just AltJSON) _piPayload
              spectrumService
          where go
                  = buildClient (Proxy :: Proxy PawsInitResource)
                      mempty
