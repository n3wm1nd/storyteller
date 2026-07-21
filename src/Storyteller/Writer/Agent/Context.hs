-- | The newtype-wrapped 'Storyteller.Context.DSL.Rendering.Context'
--   shapes an agent actually takes as a parameter, one per distinct
--   purpose -- kept in their own module (not
--   "Storyteller.Writer.Agent", the base shared-vocabulary module)
--   because "Storyteller.Context.DSL.Rendering" transitively imports
--   "Storyteller.Context.DSL.Render", which already imports
--   "Storyteller.Writer.Agent" for 'Storyteller.Writer.Agent.ContextBlock'\/
--   'Storyteller.Writer.Agent.renderEmbeddedFile" -- putting these
--   newtypes there too would be a module cycle.
--
--   Each wraps the identical underlying 'Storyteller.Context.DSL.Rendering.Context'
--   tree; the newtype is purely a label distinguishing "world context" from
--   "style" from "pinned/short-term context" at a call site and in a type
--   signature, the same reason 'Storyteller.Writer.Agent.CharLabel' wraps
--   plain 'Data.Text.Text'. An agent receiving one of these renders it
--   itself, at the point it builds its own LLM call
--   ('Storyteller.Context.DSL.Rendering.renderMessages'\/
--   'Storyteller.Context.DSL.Rendering.renderText'), rather than receiving
--   already-flattened @['UniversalLLM.Message']@\/'Storyteller.Writer.Agent.ContextBlock's
--   the way it used to -- see "Storyteller.Writer.Agent.Write"'s own
--   Haddock for why that move matters (rendering now happens where the
--   model and budget are actually known, not upstream in
--   "Server.Writer.File").
module Storyteller.Writer.Agent.Context
  ( WorldContext(..)
  , StyleContext(..)
  , PinnedContext(..)
  ) where

import Storyteller.Context.DSL.Rendering (Context)

newtype WorldContext = WorldContext Context

newtype StyleContext = StyleContext Context

newtype PinnedContext = PinnedContext Context
