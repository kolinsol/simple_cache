{application, http_interface,
 [{description, "http interface for simple cache"},
  {vsn, "0.1.0"},
  {modules, [hi_app,
             hi_sup,
             hi_server
            ]},
  {registered, [hi_sup]},
  {applications, [kernel, stdlib]},
  {mod, {hi_app, []}}
 ]}.
