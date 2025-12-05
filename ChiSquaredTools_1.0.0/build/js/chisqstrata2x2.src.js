
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the 2×2 tables (must be a factor with exactly 2 levels)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the 2×2 tables (must be a factor with exactly 2 levels)\n"}},{"name":"strata","title":"Stratifying Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the stratifying variable; each level defines one 2×2 partial table (must be a factor with 2 or more levels)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for aggregated data. If not provided, data are assumed to be in long format (one row per case).\n"}},{"name":"rowRef","title":"Row category of interest","type":"Level","variable":"(rows)","description":{"ui":"Select the row category to use as the category of interest for odds ratio calculation.","R":"The row level selected as the category of interest."}},{"name":"colRef","title":"Column category of interest","type":"Level","variable":"(cols)","description":{"ui":"Select the column category to use as the category of interest for odds ratio calculation.","R":"The column level selected as the category of interest."}},{"name":"showORAnnotation","title":"Show odds ratio interpretation beneath tables","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a plain-language interpretation of the odds ratio beneath each partial table and the marginal table.\n"}},{"name":"showPartialChiSq","title":"Show stratum-specific chi-squared results","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display chi-squared test results for each partial table in addition to the summary.\n"}},{"name":"showForestPlot","title":"Display forest plot of odds ratios","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a forest plot showing stratum-specific odds ratios with 95% confidence intervals and the common Mantel-Haenszel odds ratio.\n"}},{"name":"showDiagnosticTree","title":"Display diagnostic decision tree","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display a visual decision tree showing the diagnostic pathway based on test results.\n"}},{"name":"strataOrdered","title":"Treat stratifying variable as ordered","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), treat the stratifying variable as having a natural ordering. When enabled, allows trajectory plot visualisation and ordered axis labels.\n"}},{"name":"showTrajectoryPlot","title":"Display OR trajectory plot (if strata ordered)","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display an odds ratio trajectory plot when the stratifying variable is marked as ordered.\n"}},{"name":"showInterpretation","title":"Show interpretation guide","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display the detailed interpretation guide based on the pattern of test results.\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display detailed methodological  explanations for all tests performed.\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Stratified Analysis (2×2×K)",
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
					label: "Odds Ratio Options",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "normal",
							controls: [
								{
									type: DefaultControls.LevelSelector,
									typeName: 'LevelSelector',
									name: "rowRef",
									label: "Row category of interest",
									enable: "(rows)"
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "normal",
							controls: [
								{
									type: DefaultControls.LevelSelector,
									typeName: 'LevelSelector',
									name: "colRef",
									label: "Col. category of interest",
									enable: "(cols)"
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "normal",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "showORAnnotation"
								}
							]
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
							name: "showPartialChiSq"
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
