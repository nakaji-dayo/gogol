{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Gmail.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Gmail.Types
    (
    -- * Service Configuration
      gmailService

    -- * OAuth Scopes
    , mailGoogleComScope
    , authGmailModifyScope
    , authGmailLabelsScope
    , authGmailSendScope
    , authGmailInsertScope
    , authGmailComposeScope
    , authGmailReadonlyScope

    -- * BatchDeleteMessagesRequest
    , BatchDeleteMessagesRequest
    , batchDeleteMessagesRequest
    , bdmrIds

    -- * UsersMessagesGetFormat
    , UsersMessagesGetFormat (..)

    -- * ModifyThreadRequest
    , ModifyThreadRequest
    , modifyThreadRequest
    , mtrRemoveLabelIds
    , mtrAddLabelIds

    -- * ModifyMessageRequest
    , ModifyMessageRequest
    , modifyMessageRequest
    , mmrRemoveLabelIds
    , mmrAddLabelIds

    -- * History
    , History
    , history
    , hLabelsRemoved
    , hMessagesDeleted
    , hMessagesAdded
    , hLabelsAdded
    , hId
    , hMessages

    -- * ProFile
    , ProFile
    , proFile
    , pfMessagesTotal
    , pfThreadsTotal
    , pfHistoryId
    , pfEmailAddress

    -- * MessagePartHeader
    , MessagePartHeader
    , messagePartHeader
    , mphValue
    , mphName

    -- * ListHistoryResponse
    , ListHistoryResponse
    , listHistoryResponse
    , lhrNextPageToken
    , lhrHistory
    , lhrHistoryId

    -- * LabelType
    , LabelType (..)

    -- * UsersDraftsGetFormat
    , UsersDraftsGetFormat (..)

    -- * UsersMessagesImportInternalDateSource
    , UsersMessagesImportInternalDateSource (..)

    -- * LabelMessageListVisibility
    , LabelMessageListVisibility (..)

    -- * ListThreadsResponse
    , ListThreadsResponse
    , listThreadsResponse
    , ltrNextPageToken
    , ltrResultSizeEstimate
    , ltrThreads

    -- * MessagePart
    , MessagePart
    , messagePart
    , mpParts
    , mpBody
    , mpMimeType
    , mpHeaders
    , mpPartId
    , mpFilename

    -- * HistoryLabelAdded
    , HistoryLabelAdded
    , historyLabelAdded
    , hlaLabelIds
    , hlaMessage

    -- * ListLabelsResponse
    , ListLabelsResponse
    , listLabelsResponse
    , llrLabels

    -- * LabelLabelListVisibility
    , LabelLabelListVisibility (..)

    -- * HistoryMessageDeleted
    , HistoryMessageDeleted
    , historyMessageDeleted
    , hmdMessage

    -- * MessagePartBody
    , MessagePartBody
    , messagePartBody
    , mpbSize
    , mpbData
    , mpbAttachmentId

    -- * ListDraftsResponse
    , ListDraftsResponse
    , listDraftsResponse
    , ldrNextPageToken
    , ldrResultSizeEstimate
    , ldrDrafts

    -- * WatchResponse
    , WatchResponse
    , watchResponse
    , wrExpiration
    , wrHistoryId

    -- * UsersThreadsGetFormat
    , UsersThreadsGetFormat (..)

    -- * Draft
    , Draft
    , draft
    , dId
    , dMessage

    -- * WatchRequest
    , WatchRequest
    , watchRequest
    , wrLabelFilterAction
    , wrTopicName
    , wrLabelIds

    -- * WatchRequestLabelFilterAction
    , WatchRequestLabelFilterAction (..)

    -- * Message
    , Message
    , message
    , mRaw
    , mSnippet
    , mSizeEstimate
    , mPayload
    , mHistoryId
    , mId
    , mLabelIds
    , mThreadId
    , mInternalDate

    -- * UsersMessagesInsertInternalDateSource
    , UsersMessagesInsertInternalDateSource (..)

    -- * HistoryLabelRemoved
    , HistoryLabelRemoved
    , historyLabelRemoved
    , hlrLabelIds
    , hlrMessage

    -- * Thread
    , Thread
    , thread
    , tSnippet
    , tHistoryId
    , tId
    , tMessages

    -- * Label
    , Label
    , label
    , lThreadsUnread
    , lMessageListVisibility
    , lMessagesTotal
    , lMessagesUnread
    , lName
    , lThreadsTotal
    , lLabelListVisibility
    , lId
    , lType

    -- * ListMessagesResponse
    , ListMessagesResponse
    , listMessagesResponse
    , lmrNextPageToken
    , lmrResultSizeEstimate
    , lmrMessages

    -- * HistoryMessageAdded
    , HistoryMessageAdded
    , historyMessageAdded
    , hmaMessage
    ) where

import           Network.Google.Gmail.Types.Product
import           Network.Google.Gmail.Types.Sum
import           Network.Google.Prelude

-- | Default request referring to version 'v1' of the Gmail API. This contains the host and root path used as a starting point for constructing service requests.
gmailService :: ServiceConfig
gmailService
  = defaultService (ServiceId "gmail:v1")
      "www.googleapis.com"

-- | View and manage your mail
mailGoogleComScope :: OAuthScope
mailGoogleComScope = "https://mail.google.com/";

-- | View and modify but not delete your email
authGmailModifyScope :: OAuthScope
authGmailModifyScope = "https://www.googleapis.com/auth/gmail.modify";

-- | Manage mailbox labels
authGmailLabelsScope :: OAuthScope
authGmailLabelsScope = "https://www.googleapis.com/auth/gmail.labels";

-- | Send email on your behalf
authGmailSendScope :: OAuthScope
authGmailSendScope = "https://www.googleapis.com/auth/gmail.send";

-- | Insert mail into your mailbox
authGmailInsertScope :: OAuthScope
authGmailInsertScope = "https://www.googleapis.com/auth/gmail.insert";

-- | Manage drafts and send emails
authGmailComposeScope :: OAuthScope
authGmailComposeScope = "https://www.googleapis.com/auth/gmail.compose";

-- | View your emails messages and settings
authGmailReadonlyScope :: OAuthScope
authGmailReadonlyScope = "https://www.googleapis.com/auth/gmail.readonly";
