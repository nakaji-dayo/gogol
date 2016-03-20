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
-- Module      : Network.Google.Resource.Drive.Files.Watch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to changes to a file
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference> for @drive.files.watch@.
module Network.Google.Resource.Drive.Files.Watch
    (
    -- * REST Resource
      FilesWatchResource

    -- * Creating a Request
    , filesWatch
    , FilesWatch

    -- * Request Lenses
    , fwPayload
    , fwAcknowledgeAbuse
    , fwFileId
    ) where

import           Network.Google.Drive.Types
import           Network.Google.Prelude

-- | A resource alias for @drive.files.watch@ method which the
-- 'FilesWatch' request conforms to.
type FilesWatchResource =
     "drive" :>
       "v3" :>
         "files" :>
           Capture "fileId" Text :>
             "watch" :>
               QueryParam "acknowledgeAbuse" Bool :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Channel :> Post '[JSON] Channel
       :<|>
       "drive" :>
         "v3" :>
           "files" :>
             Capture "fileId" Text :>
               "watch" :>
                 QueryParam "acknowledgeAbuse" Bool :>
                   QueryParam "alt" AltMedia :>
                     Post '[OctetStream] Stream

-- | Subscribes to changes to a file
--
-- /See:/ 'filesWatch' smart constructor.
data FilesWatch = FilesWatch
    { _fwPayload          :: !Channel
    , _fwAcknowledgeAbuse :: !Bool
    , _fwFileId           :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilesWatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fwPayload'
--
-- * 'fwAcknowledgeAbuse'
--
-- * 'fwFileId'
filesWatch
    :: Channel -- ^ 'fwPayload'
    -> Text -- ^ 'fwFileId'
    -> FilesWatch
filesWatch pFwPayload_ pFwFileId_ =
    FilesWatch
    { _fwPayload = pFwPayload_
    , _fwAcknowledgeAbuse = False
    , _fwFileId = pFwFileId_
    }

-- | Multipart request metadata.
fwPayload :: Lens' FilesWatch Channel
fwPayload
  = lens _fwPayload (\ s a -> s{_fwPayload = a})

-- | Whether the user is acknowledging the risk of downloading known malware
-- or other abusive files. This is only applicable when alt=media.
fwAcknowledgeAbuse :: Lens' FilesWatch Bool
fwAcknowledgeAbuse
  = lens _fwAcknowledgeAbuse
      (\ s a -> s{_fwAcknowledgeAbuse = a})

-- | The ID of the file.
fwFileId :: Lens' FilesWatch Text
fwFileId = lens _fwFileId (\ s a -> s{_fwFileId = a})

instance GoogleRequest FilesWatch where
        type Rs FilesWatch = Channel
        requestClient FilesWatch{..}
          = go _fwFileId (Just _fwAcknowledgeAbuse)
              (Just AltJSON)
              _fwPayload
              driveService
          where go :<|> _
                  = buildClient (Proxy :: Proxy FilesWatchResource)
                      mempty

instance GoogleRequest (MediaDownload FilesWatch)
         where
        type Rs (MediaDownload FilesWatch) = Stream
        requestClient (MediaDownload FilesWatch{..})
          = go _fwFileId (Just _fwAcknowledgeAbuse)
              (Just AltMedia)
              driveService
          where _ :<|> go
                  = buildClient (Proxy :: Proxy FilesWatchResource)
                      mempty
