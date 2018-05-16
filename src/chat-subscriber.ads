with DDS.DomainParticipant;
with DDS.Topic;


package Chat.Subscriber is

   procedure readMessages
     (participant   : DDS.DomainParticipant.Ref_Access;
      topic         : DDS.Topic.Ref_Access);

   INIT_FAILURE : exception;

end Chat.Subscriber;
