--DROP TABLE IF EXISTS auths;


create extension citext;
create extension pgcrypto;
create table auths (
    id bigserial primary key not null,
    pass text not null,
    email citext not null unique,
    email_verification_code text not null,
    is_email_verified boolean not null
);

insert into auths(email, pass, email_verification_code, is_email_verified)
values ('boran@hotmail.com', crypt('asd', gen_salt('bf')), 'asdasd', 'f')
returning id;


UPDATE auths
SET is_email_verified = 't'
WHERE email_verification_code = 'asdasd'
returning id, email; 

SELECT * FROM auths;