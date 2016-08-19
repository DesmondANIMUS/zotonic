{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}
{% block head_extra  %}
	{% lib "js/plottable/plottable.css" "js/datetimepicker/jquery.datetimepicker.css" "js/d3/d3.min.js" "js/plottable/plottable.min.js" "js/datetimepicker/jquery.datetimepicker.full.min.js" %}
{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<form id="dtp" method="post" action="postback">
	<label for="dtp-start">Start: </label>
  	<input type="text" name="start" id="dtp-start" />

  	<label for="dtp-end">End: </label>
  	<input type="text" name="end" id="dtp-end" />

  	<input type="submit" value="Apply" />
 </form>

<br />
<svg id="erlang-memory"></svg>


<br />
{% javascript %}

	//Date Time Picker
	$('#dtp-start').datetimepicker();
	$('#dtp-end').datetimepicker();

	//Charting Code
	var data = {{ m.z_stats.datetime[{query beginDatetime="2016-8-3 15:01:46" endDatetime="2016-8-3 15:59:46"}] | to_json }};
	var emt = [];
	$.each(data, getTotalMemory);


	var xScale = new Plottable.Scales.Time();
	var yScale = new Plottable.Scales.Linear();

	var xAxis = new Plottable.Axes.Time(xScale, "bottom");
	var yAxis = new Plottable.Axes.Numeric(yScale, "left");

	var plot = new Plottable.Plots.Line();
	plot.x(timeAxisAccessor, xScale);
	plot.y(yAxisAccessor, yScale);

	var panZoom = new Plottable.Interactions.PanZoom();
	panZoom.addXScale(xScale)
			.addYScale(yScale)
			.attachTo(plot);

	var dataset = new Plottable.Dataset(emt);

	plot.addDataset(dataset);

	var chart = new Plottable.Components.Table([[yAxis, plot], [null, xAxis]]);
	chart.renderTo("svg#erlang-memory");

	function timeAxisAccessor(d) {
		var time_recorded = new Date(d.time_recorded);
		return time_recorded;
	}
	function yAxisAccessor(d) {
		//Convert result from B to Mb
		return (d.metric_value/1024/1024).toFixed(1);
	}
	function getTotalMemory(k, v) {
		if (v.metric_name === "erlang_memory__total") {
			emt.push(v);
		}
	}

{% endjavascript %}

<br />

<!-- DEBUG STUFF END -->

{% endblock %}
