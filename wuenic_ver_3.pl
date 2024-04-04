/* wuenic_ver_3.pl version 3.

Implements WHO & UNICEF rules for estimating national
infant immunization coverage. Includes explanations and
grade of confidence in estimate.

Based on methods described in:

  Burton A, Monash R, Lautenbach B, Gacic-Dobo M, Neill M, Karimov
  R, Wolfson L, Jones G, Birmingham M. WHO and UNICEF estimates of
  national infant immunization coverage: methods and processes.
  Bull World Health Organ 2009; 87:535-541.
  http://www.who.int/bulletin/volumes/87/7/08-053819.pdf

  Burton A, Gacic-Dobo M, Karimov R, Kowalski R.
  A computational logic-based representation of the WHO and UNICEF
  estimates of national immunization coverage. DRAFT 23 January 2011.

  Articles and code available at: http://sites.google.com/site/wuenic/

Author: Tony BURTON
        System Analyst
        Strategic Information Group
        Expanded Pogramme on Immunization
        Department of Immunization, Vaccines, and Biologicals
        World Health Organization
        1211 Geneva 27
        Switzerland
        BurtonA@who.int

depends upon: xsb 3.1 prolog compiler; library: lists, standard.
http://xsb.sourceforge.net/

Created:       6 November 2011.
Last update:   6 May 2016. ipv1 & rcv1 added, GoC for rcv1 based on mcv1 or mcv2 GoC

Naming conventions:
    predicates names: lower_case_underscore_separated
	function names:   lower_case_underscore_separated
    variable names:   InterCaps
    literals:         camelCase

Note that code is constructed "top down" with higher level goals appearing first.
Auxaliarity predicates and utility routines appear at the end of the module.

%TBD: verify meaning of "new vaccine" for modified right handed sawtooth criteria.
%TBD: add assign indirect value for country vaccine year (anchor? wuenic?, both?)
%TBD: Delete (or fix) bubble up GoC.
%TBD: Add green line

working group decisions:	comment
							ignoreGov, acceptGov, *modifyGov,
							ignoreAdmin, acceptAdmin, *modifyAdmin,
							ignoreReported, acceptReported, *modifiyReported,
							ignoreSurvey, acceptSurvey, modifySurvey,
							interpolate, calibrate, *use_reported,
							assignAnchor, assignWUENIC, assignGoC.

Input atomic sentences:
    admin(country,vaccine,year,percent coverage).
    gov(country,vaccine,year,percent coverge).
    legacy(country,vaccine,year,percent coverage).
    survey_results(country,vaccine,year,surveyid,description,percent coverage).
    estimate_required(country,vaccine,year,presentation,comment).
    wgd(country,vaccine,year1,year2,action,explanation1,coverageID1,coverageAssign1,coverageID2,coverageAsign2)
	births_UNPD(country,year,births)
	si_UNPD(country,year,births_surviving_to_age_one)

WHO and UNICEF working group memebers as of 1 January 2010 - 30 April 2012:
        Dr David BROWN, UNICEF/New York (dbrown@unicef.org)
        Mr Tony BURTON, WHO/Geneva  (burtona@who.int)
        Ms Marta GACIC-DOBO, WHO/Geneva  (gacicdobom@who.int)
        Mr Rouslan KARIMOV, UNICEF/NEW YORK (rkarimov@unicef.org)
        Dr Robert KOWALSKI, Imperial College London (r.kowalski@imperial.ac.uk)
 */

% use xsb tabling feature to increases inference time.
% ---------------------------------------------------
:- table anchor_point/6.
:- table survey/5.
:- table reported/5.
:- table wuenic_I/6.

:- op(500,xfy,:).


:- import member/2 from lists.
:- import length/2 from lists.
:- import sum_list/2 from lists.
:- import concat_atom/2 from string.

% sawtooth_threshold : difference in increase/decrease between Y+/-1 and Y in reported data
% survey_reported_threshold : differerence between survey results and reported data.
% reported_calibrated_threshold : if mixed anchor points swithc between using reported and calibarated.
% -----------------------------------------------------------------------------------------------------
sawtooth_threshold(10).
survey_reported_threshold(10).
confidence_survey_scope(2).
confidence_survey_threshold(10).
confidence_UNPD_threshold(10).

% establish relationship between name of first and third dose of vaccine.
% used in estimating recall bias.
% ------------------------------
vaccine(dtp3,dtp1).
vaccine(pol3,pol1).
vaccine(hib3,hib1).
vaccine(hepb3,hepb1).
vaccine(pcv3,pcv1).

admin(country,vaccine,year,coverage).
gov(country,vaccine,year,coverage).
vaccinated(country,vaccine,year,vaccinated).
target(country,vaccine,year,vaccinated).
legacy(country,vaccine,year,coverage).
survey_results(country,vaccine,year,id,description,coverage).
wgd(country,vaccine,year1,year2,action,explanation,covid1,covass1,covid2,covass2).
births_UNPD(country,year,births).
si_UNPD(country,year,surviving_infants).

% Load country-specific predicates describing data, survey_results,
% working group decisions and whether an estimate is required.
% Call estimate/0 to create country-specific estimates.
% -----------------------------------------------------
%:- ['data.pl'].
%:- estimate.
%:- halt.

% ==================================================
% top level predicate. Creates and outputs estimates.
% ==================================================
estimate :-

	% Unify country code, country name and date for output.
	% ----------------------------------------------------
	country(CountryCode,CountryName),
	date(Date),

	% Collect the set of all final estimates in list Estimate.
      % -------------------------------------------------------
	setof([
		CountryName,
		Date,
		CountryCode,
		Vaccine,
		Year,
		Coverage,
		PrevRev,
		GC,
		Admin,
		Gov,
		Reported,
		Vaccinated,
		Target,
		UnpdBirths,
		UnpdSI,
		SeriesValue,
		Source,
		SurveyInfo,
		Rule,
		Explanation],

	   wuenic(
		CountryCode,
		Vaccine,
		Year,
		Rule,
		Explanation,
		Coverage,
		PrevRev,
		GC,
		Admin,
		Gov,
		Reported,
		Vaccinated,
		Target,
		UnpdBirths,
		UnpdSI,
		Source,
		SeriesValue,
		SurveyInfo),

		Estimates),

        concat_atom(['out/', CountryCode, '.pl.v30.txt'], OutFile),
	open_out_file(Out, OutFile,	'Country\tProductionDate\tISOCountryCode\tVaccine\tYear\tWUENIC\tWUENICPreviousRevision\tGradeOfConfidence\tAdministrativeCoverage\tGovernmentEstimate\tReportedCoverage\tChildrenVaccinated\tChildrenInTarget\tBirthsUNPD\tSurvivingInfantsUNPD\tReportedTimeSeries\tReportedTimeSeriesSource\tSurveyInformation\tRule\tComment\t'),
	output_results(Estimates,Out), close(Out).

% Final estimate where there are wuenic values
% --------------------------------------------
wuenic(C,V,Y,Rule,Explanation,Coverage,PrevRev,GC,Admin,Gov,Reported,Vaccinated,Target,UnpdBirths,UnpdSI,Source,SeriesValue,SurveyInfo) :-
	estimate_required(C,V,Y,_,_),
	wuenic_I(C,V,Y,Rule,Explain,Cov),
	bound_0_100(Cov,Coverage),

	assign_Grade_of_Confidence(C,V,Y,Rule,Coverage,GoCExplanation,GC),
	%assign_GoC(C,V,Y,Rule,Coverage,GoCExplanation,GC),

	collect_data(C,V,Y,PrevRev,Admin,Gov,Reported,Vaccinated,Target,UnpdBirths,UnpdSI,_ReportedGoC,SeriesValue,Source,SurveyInfo),
	change_from_previous_revision(C,V,Y,Coverage,Change),
	collect_explanations(C,V,Y,Text),
	concat_atom([Explain,' ',Text,' ',Change,' ',GoCExplanation],Explanation).

% ------------------------------------------------------------------------------
%  End of wuenic top level routine.

	assign_Grade_of_Confidence(C,V,Y,Rule,Coverage,GoCExplanation,GC) :-
		not(member(V,['rcv1'])),
		assign_GoC(C,V,Y,Rule,Coverage,GoCExplanation,GC).

	% MG: Copy rcv1 from mcv1
	assign_Grade_of_Confidence(C,rcv1,Y,Rule,Coverage,GoCExplanation,GC) :-
		estimate_required(C,rcv1,Y,_,na),
		assign_GoC(C,mcv1,Y,Rule,Coverage,GoCExplanation,GC).

	% MG: Copy rcv1 from mcv2
	assign_Grade_of_Confidence(C,rcv1,Y,Rule,Coverage,GoCExplanation,GC) :-
		estimate_required(C,rcv1,Y,_,mcv2),
		assign_GoC(C,mcv2,Y,Rule,Coverage,GoCExplanation,GC).

	% GoC = 1 is low confidence (1 star), GoC = 3 is high confidence (3 stars) - modified 4 July 2016 TB
	% --------------------------------------------------------------------------------------------------
	assign_GoC(C,V,Y,Rule,Coverage,Support,'3') :- three_stars(C,V,Y,Rule,Coverage,Support),not(workingGroupDecision(C,V,Y,assignGoC,_,_,_)).
	assign_GoC(C,V,Y,Rule,_Coverage,Support,'2') :- two_stars(C,V,Y,Rule,Support),not(workingGroupDecision(C,V,Y,assignGoC,_,_,_)).
	assign_GoC(C,V,Y,Rule,Coverage,Support,'1') :- challenge(C,V,Y,Rule,Coverage,_),
												   setof(Evidence,challenge(C,V,Y,Rule,Coverage,Evidence),List),
												   concat_atom(['Estimate challenged by: ',List],Support),
												   not(workingGroupDecision(C,V,Y,assignGoC,_,_,_)).

	assign_GoC(C,V,Y,Rule,Coverage,Support,'1') :- no_data(C,V,Y,Rule,Coverage,Support),not(workingGroupDecision(C,V,Y,assignGoC,_,_,_)).
	assign_GoC(C,V,Y,_Rule,_,Support,GC) :- workingGroupDecision(C,V,Y,assignGoC,Explanation,_,GC),
												   concat_atom(['GoC=Assigned by working group. ',Explanation],Support).

	% Supported by reported data, survey and denominator
	% --------------------------------------------------
	three_stars(C,V,Y,Rule,_Coverage,'GoC=R+ S+ D+') :- % MG, todo: three_stars does not return Coverage
		goc_reported_condition(C,V,Y,Rule,'R+'),
		goc_survey_condition(C,V,Y,Rule,'S+'),
		goc_denominator_condition(C,V,Y,'D+').

		goc_reported_condition(C,V,Y,_Rule,'R+') :- % MG, todo: Rule = R?
			reported(C,V,Y,_,_),
			wuenic_I(C,V,Y,R,_,_),
			member(R,['R:','R: AP']).

		goc_reported_condition(C,V,Y,_Rule,'R-') :- % see above
			reported(C,V,Y,_,_),
			wuenic_I(C,V,Y,R,_,_),
			not(member(R,['R:','R: AP'])).

		goc_survey_condition(C,V,Y,Rule,'S+') :-
			supporting_survey_in_scope(C,V,Y,Rule),
			not(challenging_survey_in_scope(C,V,Y)).

		goc_survey_condition(C,V,Y,_,'S-') :-
			challenging_survey_in_scope(C,V,Y).

%  simplify previous rule to.
%			supporting_survey_in_scope(C,V,Y,Rule) :-
%				survey(C,V,Y,_,_),
%				wuenic_I(C,V,Y,'S: AP',_,_).

% rewrite rule to look at relationship between estimate rule and
% surveys in scope rule rather than difference in COV and SurveyCoverage
%
			supporting_survey_in_scope(C,V,Y,_) :-
				estimate_required(C,V,SurveyYear,_,_),
				survey(C,V,SurveyYear,_,SurveyCoverage),
				confidence_survey_scope(Scope),
				abs(Y - SurveyYear) =< Scope,
				confidence_survey_threshold(Threshold),
				wuenic_I(C,V,Y,_,_Explanation,Cov),
				abs(Cov - SurveyCoverage) =< Threshold.

% rewrite to look for surveys that challenge surveys.
% Modified 2017-05-05 Burton
%
			challenging_survey_in_scope(C,V,Y) :-
				estimate_required(C,V,SurveyYear,_,_),
				survey(C,V,SurveyYear,_,SurveyCoverage),
				confidence_survey_scope(Scope),
				abs(Y - SurveyYear) =< Scope,
				confidence_survey_threshold(Threshold),
				wuenic_I(C,V,Y,_,_Explanation,Cov),
				abs(Cov - SurveyCoverage) > Threshold.

		goc_denominator_condition(C,V,Y,'D+') :-
			goc_unpd_recal(C,V,Y),
			recal_unpd(C,V,Y,CovRec),
			confidence_UNPD_threshold(Threshold),
			wuenic_I(C,V,Y,_Rule,_Explain,Coverage),
			abs(Coverage - CovRec) < Threshold.
		goc_denominator_condition(C,V,Y,'D-') :-
			goc_unpd_recal(C,V,Y),
			recal_unpd(C,V,Y,CovRec),
			confidence_UNPD_threshold(Threshold),
			wuenic_I(C,V,Y,_Rule,_Explain,Coverage),
			abs(Coverage - CovRec) >= Threshold.

			% Ensure unpd data exist.
			% ------------------------
			goc_unpd_recal(C,V,Y) :- vaccinated(C,V,Y,_), births_UNPD(C,Y,_), si_UNPD(C,Y,_).

			% Recalculate coverage using reported number of children vaccinated
			% and births and surviving infants from UNPD estimates.
			% Births used for bcg and hepb birth dose, surviving infants
			% for remaining vaccines.
			% ------------------------
			recal_unpd(C,V,Y,CovRec) :-
				member(V,['bcg','hepbb']),
				vaccinated(C,V,Y,Vaccinated),
				births_UNPD(C,Y,Births),
				CovRec is Vaccinated / Births * 100.
				% correction of calculation for larger integers old statement = CovRec is Vaccinated * 100 / Births.
			recal_unpd(C,V,Y,CovRec) :-
				member(V,['dtp1','dtp3','pol3','ipv1','mcv1','mcv2','rcv1','hepb3','hib3','rotac','pcv3','yfv']),
				vaccinated(C,V,Y,Vaccinated),
				si_UNPD(C,Y,SI),
				CovRec is Vaccinated / SI * 100.
				% correction of calculation for larger integers old statement = CovRec is Vaccinated * 100 / SI.

	two_stars(C,V,Y,Rule,Support) :-
		(two_sources(C,V,Y,Rule,Support);
		one_source(C,V,Y,Rule,Support)),
		not(three_stars(C,V,Y,_,_,_)),
		not(challenge(C,V,Y,_,_,_)).

	% Supported by two data sources, no challenge
	% --------------------------------------------
	two_sources(C,V,Y,Rule,'GoC=R+ S+') :-
		goc_reported_condition(C,V,Y,Rule,'R+'),
		goc_survey_condition(C,V,Y,Rule,'S+').
	two_sources(C,V,Y,Rule,'GoC=S+ D+') :-
		goc_survey_condition(C,V,Y,Rule,'S+'),
		goc_denominator_condition(C,V,Y,'D+').
	two_sources(C,V,Y,Rule,'GoC=R+ D+') :-
		goc_reported_condition(C,V,Y,Rule,'R+'),
		goc_denominator_condition(C,V,Y,'D+').

	% Supported by single source, no challenge
	% -------------------------------------
	one_source(C,V,Y,Rule,'GoC=R+') :-
		not(two_sources(C,V,Y,_,_)),
		goc_reported_condition(C,V,Y,Rule,'R+').
	one_source(C,V,Y,Rule,'GoC=S+') :-
		not(two_sources(C,V,Y,_,_)),
		goc_survey_condition(C,V,Y,Rule,'S+').
	one_source(C,V,Y,_Rule,'GoC=D+') :-
		not(two_sources(C,V,Y,_,_)),
		goc_denominator_condition(C,V,Y,'D+').

	% Empirical evidence challenges estimate.
	% --------------------------------------
	challenge(C,V,Y,Rule,_Coverage,Condition) :-
		(goc_reported_condition(C,V,Y,Rule,'R-'),Condition = 'R-');
		(goc_survey_condition(C,V,Y,Rule,'S-'),Condition = 'S-');
		(goc_denominator_condition(C,V,Y,'D-'),Condition = 'D-').

	% No empirical supporting evidence.
	% --------------------------------
	no_data(C,V,Y,_Rule,_Coverage,'GoC=No accepted empirical data') :-
		not(goc_reported_condition(C,V,Y,_,_)),
		not(goc_survey_condition(C,V,Y,_,_)),
		not(goc_denominator_condition(C,V,Y,_)).

	change_from_previous_revision(C,V,Y,Coverage,'') :-
		legacy(C,V,Y,PreviousCoverage),
		PreviousCoverage = Coverage.

	change_from_previous_revision(C,V,Y,Coverage,Change) :-
		legacy(C,V,Y,PreviousCoverage),
		not(PreviousCoverage = Coverage),
		concat_atom(['Estimate of ',Coverage,' percent changed from previous revision value of ',PreviousCoverage,' percent. '],Change).

	change_from_previous_revision(C,V,Y,_,'') :-
		not(legacy(C,V,Y,_)).

% Estimate for non-DTP1 & RCV1 vaccines.
% ---------------------------------------
wuenic_I(C,V,Y,Rule,Explanation,Coverage) :-
	wuenic_II(C,V,Y,Rule,Explanation,Coverage),
	not(workingGroupDecision(C,V,Y,assignWUENIC,_,_,_)),
	not(member(V,['dtp1','rcv1'])).

% Estimate for DTP1 where DTP3 estimate <= DTP1 estimate.
% --------------------------------------------------------
wuenic_I(C,dtp1,Y,Rule,Explanation,Coverage) :-
	wuenic_II(C,dtp1,Y,Rule,Explanation,Coverage),
	not(workingGroupDecision(C,dtp1,Y,assignWUENIC,_,_,_)),
	wuenic_II(C,dtp3,Y,_,_,DTP3Coverage),
	DTP3Coverage =< Coverage.

% Estimate for DTP1 where DTP3 > DTP1: RMF
% -----------------------------------------
wuenic_I(C,dtp1,Y,'RMF:',Explanation,RMFCoverage) :-
	wuenic_II(C,dtp1,Y,_,_,DTP1Coverage),
	not(workingGroupDecision(C,dtp1,Y,assignWUENIC,_,_,_)),
	wuenic_II(C,dtp3,Y,_,_,DTP3Coverage),
	DTP3Coverage > DTP1Coverage,
	rmf(DTP3Coverage,RMFCoverage),
	concat_atom(['DTP1 coverage estimated based on DTP3 coverage of ',DTP3Coverage,'. '],Explanation).

% Estimate for DTP1 where DTP1 not reported: RMF
% -----------------------------------------
wuenic_I(C,dtp1,Y,'RMF',Explanation,RMFCoverage) :-
	wuenic_II(C,dtp3,Y,_,_,DTP3Coverage),
	not(wuenic_II(C,dtp1,Y,_,_,_)),
	not(workingGroupDecision(C,dtp1,Y,assignWUENIC,_,_,_)),
	rmf(DTP3Coverage,RMFCoverage),
	concat_atom(['Estimate based on DTP3 coverage of ',DTP3Coverage,'. '],Explanation).

% ==================
% Estimate for RCV1 where RCV1 given at MCV1.
% -------------------------------------------
wuenic_I(C,rcv1,Y,Rule,'Estimate based on estimated MCV1. ',Coverage) :-
	estimate_required(C,rcv1,Y,_,FirstRubellaDose),
	wuenic_II(C,mcv1,Y,Rule,_Explanation,Coverage),
	not(workingGroupDecision(C,rcv1,Y,assignWUENIC,_,_,_)),
	not(firstRubellaAtSecondMCV(C,rcv1,Y,FirstRubellaDose)).

% Estimate for RCV1 where RCV1 given at MCV2.
% -------------------------------------------
wuenic_I(C,rcv1,Y,Rule,'First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate',Coverage) :-
	estimate_required(C,rcv1,Y,_,FirstRubellaDose),
	wuenic_II(C,mcv2,Y,Rule,_Explanation,Coverage),
	not(workingGroupDecision(C,rcv1,Y,assignWUENIC,_,_,_)),
	firstRubellaAtSecondMCV(C,rcv1,Y,FirstRubellaDose).

% ===============
% Estimate assigned by working group.
% -----------------------------------
wuenic_I(C,V,Y,'W:',Explanation,Coverage) :-
	workingGroupDecision(C,V,Y,assignWUENIC,Explanation,_,Coverage).

% First rubella given with second measles dose
% ---------------------------------------------
firstRubellaAtSecondMCV(_C,rcv1,_Y,FirstRubellaDose) :-
  member(FirstRubellaDose,['mcv2']).


%   Preliminary estimates.
% ==============================================

% No anchor points for any year. Reported data only.
% ------------------------------------------------
wuenic_II(C,V,Y,'R:',Explain,Coverage) :-
	estimate_required(C,V,Y,_,_),
	reported_time_series(C,V,Y,Source,Coverage),
	not(anchor_point(C,V,_,_,_,_)),
	explain(ro,Source,Explain).

% Estimate at anchor points.
% --------------------------
wuenic_II(C,V,Y,Rule,Explanation,Coverage) :-
	estimate_required(C,V,Y,_,_),
	anchor_point(C,V,Y,Rule,Explanation,Coverage).

% Estimate between anchor points: between two reported anchors, gov data.
% ----------------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on coverage reported by national government. ',Coverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by reported data. ',Coverage) :-
	reported_time_series(C,V,Y,Source,Coverage),
	member(Source,['gov']),
	between_anchor_points(C,V,Y,_,RuleBefore,_,_,RuleAfter,_),
	both_anchors_resolved_to_reported(RuleBefore,RuleAfter),
	not(workingGroupDecision(C,V,Y,interpolate,_,_,_)),
	not(workingGroupDecision(C,V,Y,calibrate,_,_,_)).

% Estimate between anchor points: between two reported anchors, admin data.
% -------------------------------------------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on reported administrative data. ',Coverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by reported administrative data. ',Coverage) :-
	reported_time_series(C,V,Y,Source,Coverage),
	member(Source,['admin']),
	between_anchor_points(C,V,Y,_,RuleBefore,_,_,RuleAfter,_),
	both_anchors_resolved_to_reported(RuleBefore,RuleAfter),
	not(workingGroupDecision(C,V,Y,interpolate,_,_,_)),
	not(workingGroupDecision(C,V,Y,calibrate,_,_,_)).

% Estimate between anchor points: between two reported anchors, interpolation.
% -----------------------------------------------------------------------------
wuenic_II(C,V,Y,'R:',Explain,Coverage) :-
	reported_time_series(C,V,Y,Source,Coverage),
	member(Source,['interpolated']),
	between_anchor_points(C,V,Y,_YrBefore,RuleBefore,_,_YrAfter,RuleAfter,_),
	both_anchors_resolved_to_reported(RuleBefore,RuleAfter),
	not(workingGroupDecision(C,V,Y,interpolate,_,_,_)),
	not(workingGroupDecision(C,V,Y,calibrate,_,_,_)),
	% DWB 2023-APR concat_atom(['Estimate based on interpolation between coverage reported by national government. '],Explain).
	concat_atom(['Estimate informed by interpolation between reported data. '],Explain).

% Estimate between anchor points: calibrated
% ------------------------------------------
wuenic_II(C,V,Y,'C:',Explanation,Coverage) :-
	reported_time_series(C,V,Y,_,_ReportedCoverage),
	between_anchor_points(C,V,Y,YrBefore,RuleBefore,_,YrAfter,RuleAfter,_),
	not(both_anchors_resolved_to_reported(RuleBefore,RuleAfter)),
	not(workingGroupDecision(C,V,Y,interpolate,_,_,_)),
	calibrate(C,V,YrBefore,YrAfter,Y,Coverage),
	concat_atom(['Reported data calibrated to ',YrBefore,' and ',YrAfter,' levels. '],Explanation).

% Estimate between anchor points: interpolation forced by working group.
% ---------------------------------------------------------------------
wuenic_II(C,V,Y,'W-I:',Explanation,Coverage) :-
	between_anchor_points(C,V,Y,YrBefore,_,CoverageBefore,
							   YrAfter,_,CoverageAfter),
	workingGroupDecision(C,V,Y,interpolate,WGD_E,_,_),
	interpolate(YrBefore,CoverageBefore,YrAfter,CoverageAfter,Y,Coverage),
	% DWB 2023-APR concat_atom(['Estimate based on interpolation between ',YrBefore,' and ',YrAfter,' levels. ',WGD_E],Explanation).
	concat_atom(['Estimate informed by interpolation between ',YrBefore,' and ',YrAfter,' levels. ',WGD_E],Explanation).

% Estimate before earliest anchor: reported
% ------------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on reported data. ',ReportedCoverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by reported data. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	member(Source,['admin','gov']),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y < AnchorYear,
	not(anchor_point_earlier(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate before earliest anchor: reported-extrapolated / interpolated
% ------------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on interpolation between data reported by national government. ',ReportedCoverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by interpolation between reported data. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	member(Source,['interpolated']),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y < AnchorYear,
	not(anchor_point_earlier(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate before earliest anchor: reported-extrapolated / interpolated
% ------------------------------------------
wuenic_II(C,V,Y,'R:','Estimate based on extrapolation from data reported by national government. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	member(Source,['extrapolated']),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y < AnchorYear,
	not(anchor_point_earlier(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate before earliest anchor: calibrated
% ---------------------------------------------
wuenic_II(C,V,Y,'C:',Explanation,Coverage) :-
	reported_time_series(C,V,Y,_,ReportedCoverage),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,AnchorCoverage),
	Y < AnchorYear,
	not(anchor_point_earlier(C,V,AnchorYear)),
	not(member(AnchorRule,['R: AP'])),

	reported_time_series(C,V,AnchorYear,_,ReportedCoverageAtAnchor),
	Adj is AnchorCoverage - ReportedCoverageAtAnchor,
	Coverage is round(ReportedCoverage + Adj),
	concat_atom(['Reported data calibrated to ',AnchorYear,' levels. '],Explanation).

% Estimate after latest anchor: reported
% --------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on coverage reported by national government.',ReportedCoverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by reported data.',ReportedCoverage) :-
	reported_time_series(C,V,Y,gov,ReportedCoverage),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y > AnchorYear,
	not(anchor_point_later(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate after latest anchor: reported
% --------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on reported administrative data. ',ReportedCoverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by reported administrative data. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,admin,ReportedCoverage),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y > AnchorYear,
	not(anchor_point_later(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate after latest anchor: reported-extrapolated / interpolated
% ------------------------------------------------------------------
% DWB 2023-APR wuenic_II(C,V,Y,'R:','Estimate based on interpolation between data reported by national government. ',ReportedCoverage) :-
wuenic_II(C,V,Y,'R:','Estimate informed by interpolation between reported data. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	member(Source,['interpolated']),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y > AnchorYear,
	not(anchor_point_later(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate after latest anchor: reported-extrapolated / interpolated
% ------------------------------------------------------------------
wuenic_II(C,V,Y,'R:','Estimate based on extrapolation from data reported by national government. ',ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	member(Source,['extrapolated']),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,_),
	Y > AnchorYear,
	not(anchor_point_later(C,V,AnchorYear)),
	member(AnchorRule,['R: AP']).

% Estimate after latest anchor: calibrated
% ----------------------------------------
wuenic_II(C,V,Y,'C:',Explanation,Coverage) :-
	reported_time_series(C,V,Y,_,ReportedCoverage),
	not(anchor_point(C,V,Y,_,_,_)),

	anchor_point(C,V,AnchorYear,AnchorRule,_,AnchorCoverage),
	Y > AnchorYear,
	not(anchor_point_later(C,V,AnchorYear)),
	not(member(AnchorRule,['R: AP'])),

	reported_time_series(C,V,AnchorYear,_,ReportedCoverageAtAnchor),
	%not(reported_reason_to_exclude(C,V,Y,_,_)),
	Adj is AnchorCoverage - ReportedCoverageAtAnchor,
	Coverage is round(ReportedCoverage + Adj),
	concat_atom(['Reported data calibrated to ',AnchorYear,' levels.'],Explanation).

% DWB 2023-APR	explain(ro,gov,'Estimate based on coverage reported by national government. ').
    explain(ro,gov,'Estimate informed by reported data. ').
% DWB 2023-APR	explain(ro,admin,'Estimate based on reported administrative estimate. ').
    explain(ro,admin,'Estimate informed by reported administrative data. ').
% DWB 2023-APR	explain(ro,interpolated,'Estimate based on interpolation between reported values. ').
    explain(ro,interpolated,'Estimate informed by interpolation between reported data. ').
% DWB 2023-APR	explain(ro,extrapolated,'Estimate based on extrapolation from data reported by national government. ').
    explain(ro,extrapolated,'Estimate informed by extrapolation from reported data. ').

both_anchors_resolved_to_reported(RuleBefore,RuleAfter) :-
  member(RuleBefore,['R: AP']),
  member(RuleAfter,['R: AP']).

% =====================
% Level two processing:
%   Determine coverage value at anchor points
%   defined as years where there are multiple
%   data points (reported | survey | wgd).
%
%	Anchor point assignments:
%		Reported (gov,admin,extrapolated / interpolated timer series) supported by survey.
%		Reported not supported by survey.
%		Reported value "anchored" by working group.
%		Working group assigns anchor point value.
% =================================================

% Anchor point: survey results support reported.
% ---------------------------------------------
anchor_point(C,V,Y,'R: AP',Explanation,ReportedCoverage) :-
	reported_time_series(C,V,Y,Source,ReportedCoverage),
	survey(C,V,Y,Explain,SurveyCoverage),
	not(workingGroupDecision(C,V,Y,assignAnchor,_,_,_)),
	survey_supports_reported(ReportedCoverage,SurveyCoverage),
	explain(ap,Source,Explain,Explanation).

% Anchor point: survey results challenge reported.
% -----------------------------------------------
anchor_point(C,V,Y,'S: AP',Explanation,SurveyCoverage) :-
	reported_time_series(C,V,Y,_,ReportedCoverage),
	survey(C,V,Y,Explain,SurveyCoverage),
	not(workingGroupDecision(C,V,Y,assignAnchor,_,_,_)),
	not(survey_supports_reported(ReportedCoverage,SurveyCoverage)),
	concat_atom(['Survey evidence does not support reported data. Estimate based on survey results. ',Explain,' '],Explanation).

% Anchor point: anchor point value set to reported value
% by working group.
% -----------------------------------------------------
anchor_point(C,V,Y,'R: AP',Explanation,AssignedCoverage) :-
	reported_time_series(C,V,Y,_,AssignedCoverage),
	workingGroupDecision(C,V,Y,assignAnchor,Explanation,_,AssignedCoverage).

% Anchor point: anchor point value NOT set to reported value
% by working group.
% -----------------------------------------------------
anchor_point(C,V,Y,'W: AP',Explanation,AssignedCoverage) :-
	reported_time_series(C,V,Y,_,Coverage),
	workingGroupDecision(C,V,Y,assignAnchor,WGD_EXP,_,AssignedCoverage),
	Coverage \= AssignedCoverage,
	concat_atom(['Estimate of ',AssignedCoverage,' percent assigned by working group. ', WGD_EXP], Explanation).

	% Determine whether survey supports reported
	% -------------------------------------------
	survey_supports_reported(ReportedCoverage,SurveyCoverage) :-
		survey_reported_threshold(Threshold),
		(abs(ReportedCoverage - SurveyCoverage) =< Threshold).

	explain(ap,gov,Explain,Explanation) :-
		% DWB 2023-APR concat_atom(['Estimate based on coverage reported by national government supported by survey. ',Explain],Explanation).
		concat_atom(['Estimate informed by reported data supported by survey. ',Explain],Explanation).
	explain(ap,admin,Explain,Explanation) :-
		% DWB 2023-APR concat_atom(['Estimate based on administrative data reported by national government supported by survey. ',Explain],Explanation).
		concat_atom(['Estimate informed by reported administrative data supported by survey. ',Explain],Explanation).
	explain(ap,interpolated,Explain,Explanation) :-
		% DWB 2023-APR concat_atom(['Estimate based on interpolation between data reported by national government supported by survey. ',Explain],Explanation).
		concat_atom(['Estimate informed by interpolation between reported data supported by survey. ',Explain],Explanation).
	explain(ap,extrapolated,Explain,Explanation) :-
		concat_atom(['Estimate based on extrapolation from data reported by national government supported by survey. ',Explain],Explanation).


% ==============================================
% Level one processing:
%
%   Review data reported by national authorities
%   and survey results. Reported and survey data
%   are accepted, ignored, or modified. Modified
%   data are accepted in the modified form.
% ==============================================

% Final survey information. If multiple survey in the
% same year, accepted and modified results are averaged.
% ----------------------------------
survey(C,V,Y,Explanation,Coverage) :-
%	bagof(Cov,survey_accepted(C,V,Y,_,_,Cov),CoverageList),
	bagof(Cov,Dist^SID^survey_accepted(C,V,Y,SID,Dist,Cov),CoverageList),

	length(CoverageList,N),
	sum_list(CoverageList,SumCov),
	Coverage is round(SumCov / N),
	concat_atom(['Survey evidence of ',Coverage,' percent based on ',N, ' survey(s). '],Explanation).

% Unmodified survey results accpeted.
% -----------------------------------
survey_accepted(C,V,Y,SurveyID,_,Coverage) :-
	survey_results_for_analysis(C,V,Y,SurveyID,_,Coverage),
	not(survey_results_modified(C,V,Y,SurveyID,_,_)),
	not(survey_reason_to_exclude(C,V,Y,SurveyID,_)).

% Modified survey results accepted.
% ---------------------------------
survey_accepted(C,V,Y,SurveyID,_,ModifiedCoverage) :-
	survey_results_for_analysis(C,V,Y,SurveyID,_,_Coverage),
	survey_results_modified(C,V,Y,SurveyID,_,ModifiedCoverage),
	not(survey_reason_to_exclude(C,V,Y,SurveyID,_)).

% Survey results modified for recall bias.
% ---------------------------------------
survey_results_modified(C,V,Y,SurveyID,Explanation,ModifiedCoverage) :-
	recall_bias_modified(C,V,Y,SurveyID,Explanation,ModifiedCoverage).

% Adjust third dose for recall bias. Apply dropout observed
% between 1st and 3 dose documenented by card to history doses.
% Recalculate "card or history" based on adjustment to history.
% -------------------------------------------------------------
recall_bias_modified(C,V,Y,SurveyID,Explanation,ModifiedCoverage) :-
	member(V,['dtp3','pol3','hib3','hepb3','pcv3']),

	% Associate third dose with first dose.
	vaccine(V,FirstDose),

	% Third dose, card only
	survey_results(C,V,Y,SurveyID,DescriptionCard3Dose,C3Cov),
	member(confirm:MethodCard3Dose,DescriptionCard3Dose), member(MethodCard3Dose,['card']),
	member(age:AgeCohortCard3Dose,DescriptionCard3Dose),
	member(AgeCohortCard3Dose,['12-23 m','18-29 m','15-26 m', '24-35 m']),

	% First dose, card or history
	survey_results(C,FirstDose,Y,SurveyID,DescriptionCoH1Dose,CoH1Cov),
	member(confirm:MethodCoH1Dose,DescriptionCoH1Dose), member(MethodCoH1Dose,['card or history']),
	member(age:AgeCohortCoH1,DescriptionCoH1Dose),
	member(AgeCohortCoH1,['12-23 m','18-29 m','15-26 m','24-35 m']),

	% First dose, card only
	survey_results(C,FirstDose,Y,SurveyID,DescriptionCard1Dose,C1Cov), C1Cov > 0,
	member(confirm:MethodCard1Dose,DescriptionCard1Dose), member(MethodCard1Dose,['card']),
	member(age:AgeCohortCard1Dose,DescriptionCard1Dose),
	member(AgeCohortCard1Dose,['12-23 m','18-29 m','15-26 m','24-35 m']),

	Adj is C3Cov / C1Cov,
	ThirdHistoryAdj is ((CoH1Cov - C1Cov)*Adj),
	CovAdjustedRecall is C3Cov + ThirdHistoryAdj,

	bound_0_100(CovAdjustedRecall,ModifiedCoverage),

	survey_results_for_analysis(C,V,Y,SurveyID,_,Coverage),

	Coverage \= ModifiedCoverage,

	SurveyCoverage is round(Coverage),
%	FinalCoverage is round(ModifiedCoverage), % MG, todo: Why is
%						  % this not returned?
	CH1 is round(CoH1Cov),
	C1 is round(C1Cov),
	C3 is round(C3Cov),

	survey_results_for_analysis(C,V,Y,SurveyID,Description,_),
	member(title:Survey,Description),

	concat_atom([Survey,' card or history results of ',SurveyCoverage,' percent modifed for recall bias to ',
			ModifiedCoverage,' percent based on 1st dose card or history coverage of ',
			CH1,' percent, 1st dose card only coverage of ',C1,' percent and 3rd dose card only coverage of ',
			C3,' percent. '],Explanation).

% -------------------------------------------------
% Survey results accepted if no reason to exclude.
%
% Reasons to exclude a survey include:
%    Sample size < 300,
%    The working group decides to exclude the survey.

% --------------------------------------------------
% Reason to exclude survey: sample size < 300.
% --------------------------------------------
survey_reason_to_exclude(C,V,Y,SurveyID,Explanation) :-
	survey_results_for_analysis(C,V,Y,SurveyID,Description,_),
	member(ss:SampleSize,Description),
	SampleSize < 300,
	not(workingGroupDecision(C,V,Y,acceptSurvey,Explanation,SurveyID,_)),
	concat_atom(['Survey results ignored. Sample size ',SampleSize,' less than 300. '],Explanation).

% Reason to exclude survey: working group decision to exclude survey identified by survey id.
% -------------------------------------------------------------------------------------------
survey_reason_to_exclude(C,V,Y,SurveyID,Explain) :-
	survey_results_for_analysis(C,V,Y,SurveyID,Description,_),
	workingGroupDecision(C,V,Y,ignoreSurvey,Explanation,SurveyID,_),
	member(title:Survey,Description),
	concat_atom([Survey,' results ignored by working group. ',Explanation],Explain).

% Reason to exclude survey: working group decision to delate all surveys identified by country, vaccine, year.
% ------------------------------------------------------------------------------------------------------------
survey_reason_to_exclude(C,V,Y,SurveyID,Explain) :-
	survey_results_for_analysis(C,V,Y,SurveyID,Description,_),
	workingGroupDecision(C,V,Y,ignoreSurvey,Explanation,na,_),
	member(title:Survey,Description),
	concat_atom([Survey,' results ignored by working group. ',Explanation],Explain).

% Survey results passed for inclusion in the analysis include:
% card or history results for cohorts 12-23, 18-29, 15-26, 24-35 months of age
% --------------------------------------------------------
survey_results_for_analysis(C,V,Y,SurveyID,Description,Coverage) :-
	survey_results(C,V,Y,SurveyID,Description,Coverage),
	member(confirm:Method,Description),
	member(Method,['card or history']),
	member(age:AgeCohort,Description),
	member(AgeCohort,['12-23 m','18-29 m','15-26 m','24-35 m']).

% =============================================
% Create complete time series of reported data.
%
% Timeseries consists of:
%    coverage reported (gov or admin) by government
%    interpolated values if no reported or reported excluded between 2 years of reported.
%    value of nearest year for years no reported or reported excluded
%        before/after earliest/latest reported value (extrapolated)
% ==============================================
% Reported data.
% --------------
reported_time_series(C,V,Y,Source,Coverage) :-
		estimate_required(C,V,Y,_,_),
		reported(C,V,Y,Source,Coverage),
		not(reported_reason_to_exclude(C,V,Y,_,_)).

% Interpolation, reported data excluded between two years
% -------------------------------------------------------
reported_time_series(C,V,Y,interpolated,Coverage) :-
		estimate_required(C,V,Y,_,_),
		reported(C,V,Y,_,_),

		findall(Reason,reported_reason_to_exclude(C,V,Y,_,Reason),ReasonsToExclude),
		length(ReasonsToExclude,NReasons),
		NReasons >= 1,

		year_between_reported(C,V,Y,YearBefore,CoverageBefore,YearAfter,CoverageAfter),
		interpolate(YearBefore,CoverageBefore,YearAfter,CoverageAfter,Y,Coverage).

% Interpolation, no reported data between two years
% -------------------------------------------------
reported_time_series(C,V,Y,interpolated,Coverage) :-
		estimate_required(C,V,Y,_,_),
		not(reported(C,V,Y,_,_)),

		year_between_reported(C,V,Y,YearBefore,CoverageBefore,YearAfter,CoverageAfter),
		interpolate(YearBefore,CoverageBefore,YearAfter,CoverageAfter,Y,Coverage).

% Extrapolation, reported data for year beyond earliest / latest required estimate excluded
% ---------------------------------------------------------------------------------------
reported_time_series(C,V,Y,extrapolated,CoverageNearest) :-
		estimate_required(C,V,Y,_,_),
		reported(C,V,Y,_,_),

		findall(Reason,reported_reason_to_exclude(C,V,Y,_,Reason),ReasonsToExclude),
		length(ReasonsToExclude,NReasons),
		NReasons >= 1,

		not(year_between_reported(C,V,Y,_,_,_,_)),
		nearest_reported(C,V,Y,_YearNearest,CoverageNearest).

% Extrapolation, no reported data for year beyond earliest / latest required estimate
% ---------------------------------------------------------------------------------
reported_time_series(C,V,Y,extrapolated,CoverageNearest) :-
	estimate_required(C,V,Y,_,_),
	not(reported(C,V,Y,_,_)),
	not(year_between_reported(C,V,Y,_,_,_,_)),
	nearest_reported(C,V,Y,_YearNearest,CoverageNearest).

% =====================================
% Reasons to exclude reported data are:
%  1. Working group decision.
%  2. Coverage > 100%
%  2. Inconsistent temporal changes (sawtooth or sudden change most recent year)
% =====================================
% Reason to exclude reported: working group decision
% ---------------------------------------------------
reported_reason_to_exclude(C,V,Y,wdg,Explain) :-
	reported(C,V,Y,_,_),
	workingGroupDecision(C,V,Y,ignoreReported,Exp,_,_),
	concat_atom(['Reported data excluded. ',Exp],Explain).

% Reason to exclude reported: Reported coverage > 100%
% ----------------------------------------------------
reported_reason_to_exclude(C,V,Y,100,Explanation) :-
	reported(C,V,Y,_,Coverage),
	not(workingGroupDecision(C,V,Y,acceptReported,_,_,_)),
	Coverage > 100,
	concat_atom(['Reported data excluded because ',Coverage,' percent greater than 100 percent. '],Explanation).

% Reason to exclude reported: Sawtooth - inconsistent temporal change
% --------------------------------------------------------------------
reported_reason_to_exclude(C,V,Y,sawtooth,Explanation) :-
	reported(C,V,Y,_,Coverage),
	not(workingGroupDecision(C,V,Y,acceptReported,_,_,_)),
	YBefore is Y - 1,
	YAfter  is Y + 1,
	reported(C,V,YBefore,_,CoverageBefore),
	reported(C,V,YAfter,_,CoverageAfter),
	sawtooth_threshold(Threshold),

    % Increase
	(((Coverage - CoverageBefore) > Threshold,
	  (Coverage - CoverageAfter)  > Threshold,
	 concat_atom(['Reported data excluded due to an increase from ',CoverageBefore,' percent to ',
			Coverage,' percent with decrease ',CoverageAfter,' percent. '],Explanation))
	; % or

	%Decline
	((CoverageBefore - Coverage)  > Threshold,
	 (CoverageAfter  - Coverage)  > Threshold,
	 concat_atom(['Reported data excluded due to decline in reported coverage from ',CoverageBefore,' percent to ',
			Coverage,' percent with increase to ',CoverageAfter,' percent. '],Explanation))).

% Reason to exclude reported: sudden change in most recently reported data for classic vaccines.
% ----------------------------------------------------------------------------------------------
reported_reason_to_exclude(C,V,Y,temporalChange,Explanation) :-
	reported(C,V,Y,_,Coverage),
	not(workingGroupDecision(C,V,Y,acceptReported,_,_,_)),
	not(reported_later(C,V,Y)),
	not(member(V,['pcv3','rotac'])),
	YPrevious is Y - 1,
	reported(C,V,YPrevious,_,CoveragePreviousYear),
	sawtooth_threshold(Threshold),
	abs(CoveragePreviousYear - Coverage) > Threshold,
	concat_atom(['Reported data excluded due to sudden change in coverage from ',CoveragePreviousYear,' level to ',
			Coverage,' percent. '],Explanation).

% Reason to exclude reported: sudden decline in most recently reported data for new vaccines.
% ------------------------------------------------------------------------------------------
reported_reason_to_exclude(C,V,Y,temporalChange,Explanation) :-
	reported(C,V,Y,_,Coverage),
	not(workingGroupDecision(C,V,Y,acceptReported,_,_,_)),
	not(reported_later(C,V,Y)),
	member(V,['pcv3','rotac']),
	YPrevious is Y - 1,
	reported(C,V,YPrevious,_,CoveragePreviousYear),
	sawtooth_threshold(Threshold),
	(CoveragePreviousYear - Coverage) > Threshold,
	concat_atom(['Reported data excluded due to decline in reported coverage from ',CoveragePreviousYear,' level to ',
			Coverage,' percent. '],Explanation).

	reported_later(C,V,Year) :-
		reported(C,V,YearAfter,_,_),
		YearAfter > Year.

% ==================================================
% Level 0:
% Reported to WHO and UNICEF is government estimate.
% If government estimate missing, then reported is
% administrative data. If both missing, reported is missing.
% ---------------------------------------------------------
reported(C,V,Y,gov,Coverage) :-
	gov(C,V,Y,Coverage),
	not(workingGroupDecision(C,V,Y,ignoreGov,_,_,_)).

reported(C,V,Y,admin,Coverage) :-
	gov(C,V,Y,_),
	workingGroupDecision(C,V,Y,ignoreGov,_,_,_),
	admin(C,V,Y,Coverage),
	not(workingGroupDecision(C,V,Y,ignoreAdmin,_,_,_)).

reported(C,V,Y,admin,Coverage) :-
	admin(C,V,Y,Coverage),
	not(gov(C,V,Y,_)),
	not(workingGroupDecision(C,V,Y,ignoreAdmin,_,_,_)).

% ==========================================
% Utilities and general auxiliary predicates
% -------------------------------------------
% Determine whether a working group decision
% applies for a given year if working group
% decision applies over an interval of time.
% ------------------------------------------
workingGroupDecision(C,V,Y,Action,Explanation,SurveyID,Coverage) :-
	wgd(C,V,YBegin,YEnd,Action,Explanation,SurveyID,Coverage,_,_),
	YBegin =< Y,
	YEnd >= Y.

anchor_point_earlier(C,V,AnchorYear) :- anchor_point(C,V,Year,_,_,_), Year < AnchorYear.
anchor_point_later(C,V,AnchorYear)   :- anchor_point(C,V,Year,_,_,_), Year > AnchorYear.
anchor_point_between(C,V,YearBefore,YearAfter) :-
	anchor_point(C,V,YearBetween,_,_,_),
	YearBefore < YearBetween,
	YearAfter  > YearBetween.

% routines for interpolated points
% --------------------------------
year_between_reported(C,V,Y,YearBefore,CoverageBefore,YearAfter,CoverageAfter) :-
	reported(C,V,YearBefore,_,CoverageBefore),
	YearBefore < Y,
	not(reported_reason_to_exclude(C,V,YearBefore,_,_)),

	reported(C,V,YearAfter,_,CoverageAfter),
	YearAfter > Y,
	not(reported_reason_to_exclude(C,V,YearAfter,_,_)),
	not(reported_data_between(C,V,YearBefore,YearAfter)).

reported_data_between(C,V,YearBefore,YearAfter) :-
	reported(C,V,YearBetween,_,_),
	not(reported_reason_to_exclude(C,V,YearBetween,_,_)),
	YearBetween > YearBefore,
	YearBetween < YearAfter.

% Extrapolation using nearest neighbor
% ----------------------------------
nearest_reported(C,V,Y,YearNearest,CoverageNearest) :-
	reported(C,V,YearNearest,_,CoverageNearest),
	not(reported_reason_to_exclude(C,V,YearNearest,_,_)),
	not(reported_closer(C,V,Y,YearNearest)).

reported_closer(C,V,Y,YearNearest) :-
	reported(C,V,YearTest,_,_),
	not(reported_reason_to_exclude(C,V,YearTest,_,_)),
	abs(Y - YearTest) < abs(Y - YearNearest).

% Get values of nearest surrounding anchor points.
% -------------------------------------------------
between_anchor_points(C,V,Y,PreceedingAnchorYear,PreceedingRule,PreceedingCov,
                          SucceedingAnchorYear,SucceedingRule,SucceedingCov) :-
	not(anchor_point(C,V,Y,_,_,_)),
	anchor_point(C,V,PreceedingAnchorYear,PreceedingRule,_,PreceedingCov),
	anchor_point(C,V,SucceedingAnchorYear,SucceedingRule,_,SucceedingCov),
	PreceedingAnchorYear < Y,
	SucceedingAnchorYear > Y,
	not(anchor_point_between(C,V,PreceedingAnchorYear,SucceedingAnchorYear)).

rmf(DTP3Coverage,RMFCoverage) :-
	RMFCoverage is round(DTP3Coverage +
       (-0.0058*(DTP3Coverage * DTP3Coverage)) + (0.3912*DTP3Coverage) + 18.258).

% Provide interpolated value between two points.
% ----------------------------------------------
interpolate(EarlyYear,EarlyCoverage,LateYear,LateCoverage,Year,Coverage) :-
	Coverage is round(EarlyCoverage +
	           (Year - EarlyYear)*
			((LateCoverage - EarlyCoverage)/(LateYear - EarlyYear))).

% Calibrate reported data to anchor point value levels. Reported data at both anchors.
% -----------------------------------------------------------------------------------
calibrate(C,V,PreceedingAnchorYear,SucceedingAnchorYear,Y,Coverage) :-

	anchor_point(C,V,PreceedingAnchorYear,_,_,PreceedingCoverage),
	anchor_point(C,V,SucceedingAnchorYear,_,_,SucceedingCoverage),

	reported_time_series(C,V,PreceedingAnchorYear,_,PreceedingReportedCoverage),
	reported_time_series(C,V,SucceedingAnchorYear,_,SucceedingReportedCoverage),

	reported_time_series(C,V,Y,_,ReportedCoverage),

	interpolate(PreceedingAnchorYear,PreceedingReportedCoverage,
	            SucceedingAnchorYear,SucceedingReportedCoverage,
	            Y,ReportedInterpolated),

	interpolate(PreceedingAnchorYear,PreceedingCoverage,
	            SucceedingAnchorYear,SucceedingCoverage,
	            Y,AnchorInterpolated),

	Adj is AnchorInterpolated - ReportedInterpolated,
	Coverage is round(ReportedCoverage + Adj).

% Calibrate reported to preceeding point. Reported data only at preceeding anchor.
% ----------------------------------------------------------------------------------
calibrate(C,V,PreceedingAnchorYear,SucceedingAnchorYear,Y,Coverage) :-
	anchor_point(C,V,PreceedingAnchorYear,_,_,PreceedingCoverage),
	anchor_point(C,V,SucceedingAnchorYear,_,_,_),

	reported_time_series(C,V,PreceedingAnchorYear,_,PreceedingReportedCoverage),
	not(reported_time_series(C,V,SucceedingAnchorYear,_,_)),
	Adj is PreceedingCoverage - PreceedingReportedCoverage,

	reported_time_series(C,V,Y,_,ReportedCoverage),
	Coverage is round(ReportedCoverage + Adj).

% Ensure estimates are between 0 and 99.
% ---------------------------------------
bound_0_100(X,Y) :- X >= 0, X < 99, Y is round(X).
bound_0_100(X,Y) :- X < 0, Y is 0.
bound_0_100(X,Y) :- X >= 99, Y is 99.

% Add underlying data to each C,V,Y estimate
% ------------------------------------------
collect_data(C,V,Y,PrevRev,Admin,Gov,Reported,Vaccinated,Target,UnpdBirths,UnpdSI,_ReportedGoC,SeriesValue,Source,SurveyInfo) :-
	legacy_estimate(C,V,Y,PrevRev),
	admin_data(C,V,Y,Admin),
	gov_data(C,V,Y,Gov),
	reported_data(C,V,Y,Reported),
	vaccinated_data(C,V,Y,Vaccinated),
	target_data(C,V,Y,Target),
	time_series_data(C,V,Y,Source,SeriesValue),
	unpd_births_data(C,Y,UnpdBirths),
	unpd_si_data(C,Y,UnpdSI),
	survey_data(C,V,Y,SurveyInfo).

legacy_estimate(C,V,Y,PrevRev) :- legacy(C,V,Y,PrevRev).
legacy_estimate(C,V,Y,'') :- not(legacy(C,V,Y,_)).

admin_data(C,V,Y,Admin) :- admin(C,V,Y,Admin).                                       admin_data(C,V,Y,'') :- not(admin(C,V,Y,_)).
gov_data(C,V,Y,Gov) :- gov(C,V,Y,Gov).                                               gov_data(C,V,Y,'') :- not(gov(C,V,Y,_)).
reported_data(C,V,Y,Reported) :- reported(C,V,Y,_,Reported).                         reported_data(C,V,Y,'') :- not(reported(C,V,Y,_,_)).
vaccinated_data(C,V,Y,Vaccinated) :- vaccinated(C,V,Y,Vaccinated).                   vaccinated_data(C,V,Y,'') :- not(vaccinated(C,V,Y,_)).
target_data(C,V,Y,Target) :- target(C,V,Y,Target).                                   target_data(C,V,Y,'') :- not(target(C,V,Y,_)).
unpd_births_data(C,Y,UnpdBirths) :- births_UNPD(C,Y,UnpdBirths).                     unpd_births_data(C,Y,'') :- not(births_UNPD(C,Y,_)).
unpd_si_data(C,Y,UnpdSI) :- si_UNPD(C,Y,UnpdSI).                                     unpd_si_data(C,Y,'') :- not(si_UNPD(C,Y,_)).

time_series_data(C,V,Y,Source,SeriesValue) :- reported_time_series(C,V,Y,Source,SeriesValue).
time_series_data(C,V,Y,'','') :- not(reported_time_series(C,V,Y,_,_)).

survey_data(C,V,Y,SurveyInfo) :- survey(C,V,Y,_,SurveyInfo).
survey_data(C,V,Y,'') :- not(survey(C,V,Y,_,_)).

% Collection explanations.
% -----------------------
collect_explanations(C,V,Y,Explanations) :-
	findall(Text,explanation(C,V,Y,Text),Explanations).

explanation(C,V,Y,Text) :- survey_reason_to_exclude(C,V,Y,_,Text).
explanation(C,V,Y,Text) :- survey_results_modified(C,V,Y,_,Text,_).
explanation(C,V,Y,Text) :- reported_reason_to_exclude(C,V,Y,_,Text).
explanation(C,V,Y,Text) :- workingGroupDecision(C,V,Y,comment,Text,_,_).
explanation(C,V,Y,Text) :- workingGroupDecision(C,V,Y,acceptSurvey,Text,_,_).
explanation(C,V,Y,Text) :- workingGroupDecision(C,V,Y,acceptReported,Text,_,_).
explanation(C,V,Y,Text) :- workingGroupDecision(C,V,Y,ignoreGov,Text,_,_).
%explanation(C,V,Y,Text) :- workingGroupDecision(C,V,Y,interpolate,Text,_,_).

open_out_file(Out,File,Header) :-
	open(File,write,Out),
	write(Out,Header),
	nl(Out).

output_results([],_).
output_results([H|T],Out) :- output_fields(H,Out), output_results(T,Out).

output_fields([],Out) :- nl(Out).
output_fields([H|T],Out) :- write(Out,H),write(Out,'\t'),output_fields(T,Out).
