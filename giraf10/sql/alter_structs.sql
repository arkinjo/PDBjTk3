alter table structs add column updated boolean default true;
update structs set updated = false;
