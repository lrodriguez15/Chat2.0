with DDS;

package Chat is
   use DDS;

   DEFAULT_DOMAIN_ID : constant := 0;
   DOMAIN_ID_MAX     : constant := 250;
   TOPIC_NAME        : constant DDS.String := DDS.To_DDS_String ("CHAT");

   FAILURE : exception;

   type App_Mode is
     (APP_MODE_UNDEFINED,
      APP_MODE_PUBLISHER,
      APP_MODE_SUBSCRIBER);

   type Chat_Room is
     (SPORTS,
      BOOKS,
      MUSIC,
      CUSTOM);

   type CommandLineArguments is record
      domainId      : DDS.Domainid_T;
      mode          : App_Mode;
   end record;

   procedure usage;

   function selectChatRoom return Chat_Room;

   procedure startApplication (arg : CommandLineArguments);

end Chat;
