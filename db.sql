CREATE TABLE rsvp (
   id serial primary key not null,
   name varchar(255),
   guest varchar(255),
   email varchar(255),
   attending varchar(255),
   bus varchar(255),
   dietry text,
   message text
);
