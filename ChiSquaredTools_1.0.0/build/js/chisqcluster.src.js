
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the contingency table (must be a factor)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the contingency table (must be a factor)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for wide-format data. If not provided, data are assumed to be in long format (one row per case).\n"}},{"name":"plotRowDendro","title":"Plot row dendrogram","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display hierarchical clustering dendrogram  for table rows with critical value cutoff line\n"}},{"name":"plotColDendro","title":"Plot column dendrogram","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display hierarchical clustering dendrogram  for table columns with critical value cutoff line\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display detailed methodological  explanations for the clustering procedure\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Contingency Table Row/Column Clustering",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Row Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "rows",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Column Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "cols",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Counts (optional)",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "counts",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Plot Options",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "plotRowDendro"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "plotColDendro"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Display Options",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showMethodInfo"
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
