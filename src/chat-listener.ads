with DDS;
with DDS.DataReaderListener;

package Chat.Listener is

   type Ref is new DDS.DataReaderListener.Ref with record
      receiving : Boolean := True;
   end record;

   type Ref_Access is access all Ref'Class;

   procedure On_Data_Available
     (Self       : not null access Ref;
      The_Reader : in DDS.DataReaderListener.DataReader_Access);

end Chat.Listener;
