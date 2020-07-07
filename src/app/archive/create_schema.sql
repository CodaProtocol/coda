CREATE TABLE public_keys
( id    serial PRIMARY KEY
, value text   NOT NULL UNIQUE
);

CREATE INDEX idx_public_keys_value ON public_keys(value);

CREATE TABLE snarked_ledger_hashes
( id    serial PRIMARY KEY
, value text   NOT NULL UNIQUE
);

CREATE INDEX idx_snarked_ledger_hashes_value ON snarked_ledger_hashes(value);

CREATE TYPE user_command_type AS ENUM ('payment', 'delegation', 'create_token', 'create_account', 'mint_tokens', 'set_token_permissions', 'set_account_permissions');

CREATE TABLE user_commands
( id           serial            PRIMARY KEY
, type         user_command_type NOT NULL
, fee_payer_id int               NOT NULL REFERENCES public_keys(id)
, source_id    int               NOT NULL REFERENCES public_keys(id)
, receiver_id  int               NOT NULL REFERENCES public_keys(id)
, fee_token    text              NOT NULL
, token        text              NOT NULL
, nonce        bigint            NOT NULL
, amount       bigint
, fee          bigint            NOT NULL
, memo         text              NOT NULL
, hash         text              NOT NULL UNIQUE
);

CREATE TYPE internal_command_type AS ENUM ('fee_transfer', 'coinbase');

CREATE TABLE internal_commands
( id          serial                PRIMARY KEY
, type        internal_command_type NOT NULL
, receiver_id int                   NOT NULL REFERENCES public_keys(id)
, fee         bigint                NOT NULL
, token       text                  NOT NULL
, hash        text                  NOT NULL UNIQUE
);

CREATE TABLE blocks
( id                     serial PRIMARY KEY
, state_hash             text   NOT NULL UNIQUE
, parent_id              int                    REFERENCES blocks(id)
, creator_id             int    NOT NULL        REFERENCES public_keys(id)
, snarked_ledger_hash_id int    NOT NULL        REFERENCES snarked_ledger_hashes(id)
, ledger_hash            text   NOT NULL
, height                 bigint NOT NULL
, timestamp              bigint NOT NULL
, coinbase_id            int                    REFERENCES internal_commands(id)
);

CREATE INDEX idx_blocks_state_hash ON blocks(state_hash);
CREATE INDEX idx_blocks_creator_id ON blocks(creator_id);
CREATE INDEX idx_blocks_height     ON blocks(height);

CREATE TABLE blocks_user_commands
( block_id        int NOT NULL REFERENCES blocks(id)
, user_command_id int NOT NULL REFERENCES user_commands(id)
, PRIMARY KEY (block_id, user_command_id)
);

CREATE TABLE blocks_internal_commands
( block_id            int NOT NULL REFERENCES blocks(id)
, internal_command_id int NOT NULL REFERENCES internal_commands(id)
, PRIMARY KEY (block_id, internal_command_id)
);
