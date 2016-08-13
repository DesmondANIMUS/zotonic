{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}
{% block head_extra  %}
	{% lib "js/d3/d3.min.js" "js/plottable/plottable.min.js" "js/plottable/plottable.css" %}
{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<br />

<svg id="chart"></svg>

{% javascript %}

	var xScale = new Plottable.Scales.Linear();
				var yScale = new Plottable.Scales.Linear();

				var xAxis = new Plottable.Axes.Numeric(xScale, "bottom");
				var yAxis = new Plottable.Axes.Numeric(yScale, "left");

				var plot = new Plottable.Plots.Line();
				plot.x(function (d) {return d.x; }, xScale);
				plot.y(function (d) {return d.y; }, yScale);

				var data = [
							  { "x": 0, "y": 1 },
							  { "x": 1, "y": 2 },
							  { "x": 2, "y": 4 },
							  { "x": 3, "y": 8 }
							];

				var dataset = new Plottable.Dataset(data);

				plot.addDataset(dataset);

				var chart = new Plottable.Components.Table([[yAxis, plot], [null, xAxis]]);
				chart.renderTo("svg#chart");

{% endjavascript %}

<br />

<!-- {% print m.z_stats.datetime[{query beginDatetime="2016-8-3 15:01:46" endDatetime="2016-8-3 15:59:46"}] %} -->

<br />

<!-- DEBUG STUFF END -->

{% endblock %}
