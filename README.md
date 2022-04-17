signerbot
=====

An OTP application

Build
-----

    $ rebar3 compile

-----

    Для работы приложения нужно создать папку "resources", в котором должны быть:
        1. apksigner и все его зависимости
        2. sign_data.json, который должен быть следующего формата:
            {
                "key_alias": "something",
                "password": "qwe123",
                "keystore": "test_keystore"
            }
        3. файл keystore
        4. в папке src должен быть файл params_bot.hrl, в котором обязательно должны храниться две записи следующего формата
            -define(PASSWORD, <<"my_password">>).
            -define(TOKEN, "my_token").

    В случае сложностей с созданием папок и файлов, можно посмотреть пример в ветке "example"

    Для запуска приложения нужно вызвать комманду:
        rebar3 shell
