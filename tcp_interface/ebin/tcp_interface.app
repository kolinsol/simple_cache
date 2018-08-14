{application, tcp_interface,
 [{description, "tcp interface for simple cache"},
  {vsn, "0.1.0"},
  {modules, [ti_app,
             ti_sup,
             ti_server_sup,
             ti_server,
             ti_event,
             ti_event_logger]},
  {registered, [ti_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ti_app, []}}
 ]}.
