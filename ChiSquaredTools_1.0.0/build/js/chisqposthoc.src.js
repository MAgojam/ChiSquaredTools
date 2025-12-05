
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the contingency table (must be a factor)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the contingency table (must be a factor)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for wide-format data. If not provided, data are assumed to be in long format (one row per case). The variable will be treated as numeric frequencies.\n"}},{"name":"stdres","title":"Standardised Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute standardised residuals\n"}},{"name":"momcorrstdres","title":"Moment-Corrected Standardised Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute moment-corrected standardised residuals\n"}},{"name":"adjstdres","title":"Adjusted Standardised Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute adjusted standardised residuals\n"}},{"name":"quetelet","title":"Quetelet Index","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute Quetelet Index\n"}},{"name":"ij","title":"IJ Association Factor","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute IJ association factor\n"}},{"name":"bsOutlier","title":"Backwards-Stepping Outlier Detection","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), perform backwards-stepping outlier detection using deleted residuals and G² drop statistics. Identifies cells that deviate significantly from independence, with protection against masking and swamping effects. Method from Simonoff (1988).\n"}},{"name":"bsKmax","title":"Maximum Outliers to Test (k_max, cell count)","type":"Integer","min":1,"max":100,"default":5,"description":{"R":"maximum number of potential outlier cells to test in backwards-stepping procedure (default: 5). Simonoff (1988) suggests 20-30% of cells. Higher values increase computation time but do not inflate false positive rates due to Bonferroni correction.\n"}},{"name":"pem","title":"PEM / Sakoda's D Local","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute PEM (Percentage of Maximum deviation) with bootstrap confidence intervals. PEM is mathematically equivalent to Sakoda's cell-level index D_ij, expressed as a percentage rather than proportion.\n"}},{"name":"medpolish","title":"Standardised Median Polish Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute standardised median polish residuals\n"}},{"name":"adjmedpolish","title":"Adjusted Standardised Median Polish Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute adjusted standardised median polish residuals\n"}},{"name":"gkres","title":"Goodman-Kruskal Residuals","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute Goodman-Kruskal residuals. These residuals measure how knowing the predictor category changes the probability of each response category relative to its marginal probability. Two tables are produced: one treating columns as predictor (rows as response), one treating rows as predictor (columns as response).\n"}},{"name":"dep","title":"Dependence Evaluator Proportion (DEP)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute DEP (Dependence Evaluator Proportion). This measure requires designating one column category as the outcome of interest and computes a normalised probability difference for each row category.\n"}},{"name":"depOutcome","title":"Outcome Category (for DEP)","type":"Level","variable":"(cols)","description":{"R":"the column category to treat as the outcome of interest for DEP analysis. Levels are populated from the column variable.\n"}},{"name":"sidakCorrection","title":"Apply Šidák correction (for std./moment-corr./adj. std. residuals)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), apply Šidák correction to  standardised, moment-corrected standardised, and adjusted standardised residual significance thresholds\n"}},{"name":"confLevel","title":"Confidence Level (for PEM)","type":"Number","min":0.5,"max":0.999,"default":0.95,"description":{"R":"confidence level for PEM bootstrap confidence intervals (default: 0.95)\n"}},{"name":"bootstrapReps","title":"Bootstrap Replications (for PEM)","type":"Integer","min":100,"max":10000,"default":999,"description":{"R":"number of bootstrap replications for PEM confidence intervals (default: 2000)\n"}},{"name":"seed","title":"Random Seed","type":"Integer","min":1,"max":999999,"default":123,"description":{"R":"random seed for reproducibility (default: 123)\n"}},{"name":"showSignificanceTables","title":"Show significance summary tables","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display tables summarising significant positive, negative, and non-significant cell patterns\n"}},{"name":"showPemPlot","title":"Show PEM forest plot","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display a diverging bar chart of PEM values with bootstrap confidence intervals, sorted by magnitude\n"}},{"name":"pemPlotFilter","title":"PEM plot filter","type":"List","options":[{"name":"all","title":"All cells"},{"name":"sigOnly","title":"Significant only"},{"name":"nonsigOnly","title":"Non-significant only"}],"default":"all","description":{"R":"filter which cells to display in the PEM forest plot: 'all' (default), 'sigOnly', or 'nonsigOnly'\n"}},{"name":"showDepPlot","title":"Show DEP forest plot","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display a forest plot of DEP values with significance bounds derived from chi-squared critical values\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display detailed methodological  explanations for all selected metrics\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Chi-Squared Post-Hoc Analysis",
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
					label: "Symmetric Measures",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "stdres"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "momcorrstdres"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "adjstdres"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "quetelet"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "ij"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "bsOutlier"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "pem"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "medpolish"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "adjmedpolish"
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
					label: "Directional Measures",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "gkres"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "dep"
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
					label: "Adjustment for Multiple Comparisons",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "sidakCorrection"
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
					label: "Backwards-Stepping Options",
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "bsKmax",
							format: FormatDef.number,
							enable: "(bsOutlier)"
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
					label: "PEM Options",
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "confLevel",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "bootstrapReps",
							format: FormatDef.number
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "seed",
							format: FormatDef.number
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showPemPlot"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "pemPlotFilter"
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
					label: "DEP Options",
					controls: [
						{
							type: DefaultControls.LevelSelector,
							typeName: 'LevelSelector',
							name: "depOutcome",
							enable: "(dep)"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showDepPlot",
							enable: "(dep)"
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
							name: "showSignificanceTables"
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
