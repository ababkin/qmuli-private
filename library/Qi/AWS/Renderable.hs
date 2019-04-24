module Qi.AWS.Renderable where

import           Stratosphere (Resource)

import Qi.AWS.Types


class AwsResource a => Renderable a where
  render :: AppName -> (LogicalId (ResourceType a), a) -> Resource
