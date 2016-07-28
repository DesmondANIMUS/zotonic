{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<br />

{{ m.acl.user }}

<br />

{{ m.z_stats.datetime["{{2016, 7, 28}, {10, 23, 0}}"] }}

<br />

<!-- DEBUG STUFF END -->

{% endblock %}
