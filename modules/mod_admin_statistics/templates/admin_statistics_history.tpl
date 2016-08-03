{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<br />

{{ m.acl.user }}

<br />


{{ m.z_stats.datetime[{query beginDatetime="2016-8-2 00-01-46" endDatetime="2016-8-8 23-59-46"}] }}

{% for id,time,metric_name,metric_value in m.z_stats.datetime[{query beginDatetime="2016-8-2 00-01-46" endDatetime="2016-8-8 23-59-46"}] %}

    {% for k,v in id %}

    {{ k }} : {{ v }}

    {% endfor %} 

{% endfor %}
<br />

<!-- DEBUG STUFF END -->

{% endblock %}
