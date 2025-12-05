
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"rows","title":"Row Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for rows in the contingency table (must be a factor)\n"}},{"name":"cols","title":"Column Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"a string naming the variable for columns in the contingency table (must be a factor)\n"}},{"name":"counts","title":"Counts (optional)","type":"Variable","suggested":["continuous"],"description":{"R":"optional variable containing frequency counts for wide-format data. If not provided, data are assumed to be in long format (one row per case).\n"}},{"name":"phi","title":"Phi (2×2 only)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Phi coefficient for 2×2 tables\n"}},{"name":"phiSigned","title":"Phi signed (2×2 only)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute signed Phi coefficient for 2×2 tables\n"}},{"name":"phiCorrected","title":"Phi corrected (2×2 only)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute corrected Phi coefficient for 2×2 tables\n"}},{"name":"coleC7","title":"Cole's C7 (2×2 only)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Cole's C7 coefficient with Ratliff's (1982) correction for 2×2 tables. A margin-free measure that normalises the cross-product difference by its maximum achievable value given the marginals.\n"}},{"name":"zysnoPhi","title":"Zysno's φ* (2×2 only)","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Zysno's (1997) phi-star coefficient for 2×2 tables. A margin-free measure that reduces phi's dependence on marginal distributions while retaining sensitivity to association strength.\n"}},{"name":"contingencyC","title":"Contingency Coefficient C","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute contingency coefficient C\n"}},{"name":"cAdjusted","title":"C adjusted","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute adjusted contingency coefficient C\n"}},{"name":"cCorrected","title":"C corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute maximum-corrected contingency coefficient C\n"}},{"name":"cramersV","title":"Cramér's V","type":"Bool","default":true,"description":{"R":"TRUE (default) or FALSE, compute Cramér's V with confidence interval\n"}},{"name":"vCorrected","title":"Cramér's V corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute maximum-corrected Cramér's V\n"}},{"name":"vStandardised","title":"Cramér's V standardised","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Cramér's V on standardised table\n"}},{"name":"vBiasCorrected","title":"Cramér's V bias-corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute bias-corrected Cramér's V\n"}},{"name":"cohenW","title":"Cohen's w","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Cohen's w\n"}},{"name":"wHat","title":"W-hat coefficient","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute W-hat coefficient with confidence interval\n"}},{"name":"wHatCorrected","title":"W-hat corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute maximum-corrected W-hat coefficient. Corrects for the maximum achievable value in tables with asymmetric marginals.\n"}},{"name":"sakoda","title":"Sakoda's D Global","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), Sakoda's generalized index of dissimilarity (D_G), a global measure of association representing the proportion of cases needing redistribution to achieve independence.\n"}},{"name":"sakodaCorrected","title":"Sakoda's D Global corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute maximum-corrected Sakoda's D Global. Corrects for the maximum achievable value in tables with asymmetric marginals.\n"}},{"name":"sakodaLocal","title":"Sakoda's D Local / PEM","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), Sakoda's cell-level index of dissimilarity (D_ij), mathematically equivalent to Cibois's PEM (Percentage of Maximum deviation). Displays a table of local association values without confidence intervals. For bootstrap CIs, use the Post-hoc Analysis facility.\n"}},{"name":"yulesQ","title":"Yule's Q","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Yule's Q with confidence interval and p-value. For 2×2 tables: single value. For larger tables: pairwise comparisons.\n"}},{"name":"yulesY","title":"Yule's Y","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Yule's Y with confidence interval and p-value. For 2×2 tables: single value. For larger tables: pairwise comparisons.\n"}},{"name":"oddsRatio","title":"Odds Ratio","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Odds Ratio with confidence interval and p-value. For 2×2 tables: single value. For larger tables: independent odds ratios.\n"}},{"name":"gkLambda","title":"Goodman-Kruskal Lambda","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Goodman-Kruskal Lambda (asymmetric and symmetric) with confidence intervals\n"}},{"name":"gkLambdaCorrected","title":"Goodman-Kruskal Lambda corrected","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute corrected Goodman-Kruskal Lambda\n"}},{"name":"gkTau","title":"Goodman-Kruskal Tau","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), compute Goodman-Kruskal Tau (asymmetric) with confidence interval\n"}},{"name":"confLevel","title":"Confidence Level","type":"Number","min":0.5,"max":0.999,"default":0.95,"description":{"R":"confidence level for confidence intervals (default: 0.95)\n"}},{"name":"bootstrapReps","title":"Bootstrap Replications","type":"Integer","min":100,"max":10000,"default":999,"description":{"R":"number of bootstrap replications for computing confidence intervals. Applies to: Phi corrected, C adjusted, C corrected, V corrected, W-hat, W-hat corrected, Sakoda's D, and Sakoda's D corrected. Default: 999. See table annotation for details.\n"}},{"name":"seed","title":"Random Seed","type":"Integer","min":1,"max":999999,"default":123,"description":{"R":"random seed for reproducibility (default: 123)\n"}},{"name":"showChisqMax","title":"Show chi-squared-maximising table","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display the chi-square-maximising table\n"}},{"name":"showStandardised","title":"Show standardised table","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display the standardised contingency table\n"}},{"name":"showThresholds","title":"Show effect size threshold table","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display effect size interpretation thresholds\n"}},{"name":"showMethodInfo","title":"Show detailed method explanations","type":"Bool","default":false,"description":{"R":"TRUE or FALSE (default), display detailed methodological explanations for selected measures\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Measures of Association and Effect Sizes",
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
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Chi-squared-based Measures",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":0},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "phi"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":0},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "phiCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":1},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "phiSigned"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":2},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "coleC7"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":3},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "zysnoPhi"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":4},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "contingencyC"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":4},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cAdjusted"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":5},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":6},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cramersV"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":6},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "vCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":7},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "vStandardised"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":8},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "vBiasCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":9},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cohenW"
										}
									]
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
					label: "Distance-based Measures",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							stretchFactor: 1,
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":0},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "wHat"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":0},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "wHatCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":0,"row":1},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "sakoda"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":1,"row":1},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "sakodaCorrected"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									cell: {"column":2,"row":1},
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "sakodaLocal"
										}
									]
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
					label: "Margin-free Measures",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "yulesQ"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "yulesY"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "oddsRatio"
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
					label: "PRE Measures",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "gkLambda"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "gkLambdaCorrected"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "gkTau"
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
					name: "confLevel",
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
					label: "Bootstrap Replications (see table note for applicable measures)",
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "bootstrapReps",
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
							name: "showChisqMax"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showStandardised"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "showThresholds"
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
