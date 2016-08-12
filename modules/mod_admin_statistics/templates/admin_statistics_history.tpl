{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<br />

{{ m.acl.user }}

<br />


{% print m.z_stats.datetime[{query beginDatetime="2016-8-3 00:01:46" endDatetime="2016-8-3 15:59:46"}] %}

<br />

<!-- DEBUG STUFF END -->

{% endblock %}
