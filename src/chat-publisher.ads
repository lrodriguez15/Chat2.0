with DDS;
with DDS.DomainParticipant;
with DDS.Topic;
with Ada.Strings.Unbounded;
with DDS.Builtin_String_DataWriter;

package Chat.Publisher is

   package SU   renames Ada.Strings.Unbounded;
   INIT_FAILURE : exception;

   DELAY_TIME : duration := 1.0;

   type ChatPublisher is tagged private;

   type ChatMessage is record
      nickname : aliased Standard.DDS.String; --  maximum length = (128)
      message : aliased Standard.DDS.String; --  maximum length = (1024)
   end record;

   procedure Publish (pub : ChatPublisher);

   function Init (participant : DDS.DomainParticipant.Ref_Access;
                  topic       : DDS.Topic.Ref_Access;
                  nickname : SU.Unbounded_String
                 )
                  return ChatPublisher;

private

   type ChatPublisher is tagged record
      writer : DDS.Builtin_String_DataWriter.Ref_Access;
      nickname : SU.Unbounded_String;
      topic : DDS.Topic.Ref_Access;
   end record;
end Chat.Publisher;