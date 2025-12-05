
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the contingency table (must be a factor)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the contingency table (must be a factor)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for wide-format data. If not provided, data are assumed to be in long format (one row per case).\n"}},{"name":"traditionalTest","title":"Traditional Chi-Squared Test","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), compute traditional chi-squared test\n"}},{"name":"n1Test","title":"(N-1) Adjusted Chi-Squared Test","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute (N-1)/N adjusted chi-squared test\n"}},{"name":"permTest","title":"Permutation Test","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute permutation-based test\n"}},{"name":"monteCarloTest","title":"Monte Carlo Test","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute Monte Carlo test\n"}},{"name":"mTest","title":"M Test","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), compute M test based on maximum adjusted residual (Fuchs & Kenett, 1980). Uses Bonferroni correction for multiple comparisons. May have higher power than chi-squared when association is driven by a single outlying cell.\n"}},{"name":"nPerms","title":"Number of Permutations/Monte Carlo Replications","type":"Integer","min":99,"max":9999,"default":999,"description":{"R":"number of permutations or Monte Carlo replications (default: 999)\n"}},{"name":"seed","title":"Random Seed","type":"Integer","min":1,"max":999999,"default":123,"description":{"R":"random seed for reproducibility (default: 123)\n"}},{"name":"showMethodGuidance","title":"Show method selection guidance","type":"Bool","default":true,"description":{"R":"TRUE or FALSE (default: TRUE), display guidance for selecting an appropriate testing method based on table characteristics\n"}},{"name":"showDistributions","title":"Show distribution plots","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), plot distributions of permutation and Monte Carlo test statistics\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default: FALSE), display detailed methodological  explanations for all selected tests\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Chi-Squared Test of Independence",
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
					label: "Test Methods",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "traditionalTest"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "n1Test"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "permTest"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "monteCarloTest"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "mTest"
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
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "nPerms",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "seed",
					format: FormatDef.number
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
							name: "showMethodGuidance"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showDistributions",
							enable: "(permTest || monteCarloTest)"
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
