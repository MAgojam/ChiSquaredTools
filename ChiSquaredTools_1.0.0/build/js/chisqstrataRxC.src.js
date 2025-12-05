
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the contingency tables (must be a factor with 2 or more levels)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the contingency tables (must be a factor with 2 or more levels)\n"}},{"name":"strata","title":"Stratifying Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the stratifying variable; each level defines one partial table (must be a factor with 2 or more levels)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for aggregated data. If not provided, data are assumed to be in long format (one row per case).\n"}},{"name":"nBootstrap","title":"Number of bootstrap replicates","type":"Integer","default":1000,"min":100,"max":10000,"description":{"R":"number of bootstrap replicates for computing confidence intervals of V corrected (default: 1000)\n"}},{"name":"showResiduals","title":"Show adjusted standardised residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display adjusted standardised  residuals for each partial table. Residuals exceeding ±1.96  (highlighted) indicate cells contributing significantly to  the chi-squared statistic at alpha = 0.05.\n"}},{"name":"showForestPlot","title":"Display forest plot of V corrected","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a forest plot showing stratum-specific V corrected with 95% confidence intervals and the weighted average.\n"}},{"name":"showDiagnosticTree","title":"Display diagnostic decision tree","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a visual decision tree showing the diagnostic pathway based on test results.\n"}},{"name":"strataOrdered","title":"Treat stratifying variable as ordered","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), treat the stratifying variable as having a natural ordering. When enabled, allows trajectory plot visualisation.\n"}},{"name":"showTrajectoryPlot","title":"Display V corrected trajectory plot (if strata ordered)","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a V corrected trajectory plot when the stratifying variable is marked as ordered.\n"}},{"name":"showInterpretation","title":"Show interpretation guide","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display the detailed interpretation guide based on the pattern of test results.\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display detailed methodological  explanations for all tests performed.\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Stratified Analysis (R×C×K)",
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
					label: "Stratifying Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "strata",
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
					label: "Bootstrap Settings",
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "nBootstrap",
							format: FormatDef.number
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
					label: "Visual Summaries",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showResiduals"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showForestPlot"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "strataOrdered"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showTrajectoryPlot",
							enable: "(strataOrdered)",
							style: "list-inline"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showDiagnosticTree"
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
					label: "Interpretive Guides",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showInterpretation"
						},
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
