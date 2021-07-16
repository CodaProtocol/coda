## Summary
[summary]: #summary

The RFC (Request For Comments) is a process by which we share technical designs and gain consensus on how to proceed. It is intended to provide a consistent and controlled path for new features to enter the project. While the RFCs can be made by anyone, including community members, RFCs that don’t come from the core engineering team will need a champion to drive the RFC through the consensus process. 

## Motivation
[motivation]: #motivation

What problems are we tring to address?

- Long Lived Open RFC Prs - We don’t have a good process for closing out RFCs, as a consequence, there are quite a few open RFCs (19) the majority of which are not active. It’s also pretty co
- RFCs often don’t get engagement or feedback

## Detailed design
[detailed-design]: #detailed-design

**When to write an RFC?**

RFCs should be created for “substantial” changes to the protocol, user facing components (CLI, API, etc), the infrastructure or processes. Examples:

- New Infrastructure Tools
- A new feature that will take a full sprint or more to implement
- Changes to existing features that make no trivial changes to the functionality.

Not sure if something needs an RFC?  Consult with the team during dev discussions.  

**When to not follow this process?**

- Bug fixes, unless the fix requires foundational changes to the way the protocol works and/or users interact with the product
- Confidential work - in which case RFCs should be in Notion

**Owner/Champion**

Regardless if it's one person or a focused group of people who are generating the RFC, there should be an owner or champion who pushes the RFC forward.  In most cases, this is be the person writing the RFC.

**The RFC Iteration Process**

- Before submitting an RFC it is generally a good idea to pursue feedback from other project developers beforehand, to ascertain that the RFC and approach is desirable; having a consistent impact on the project requires concerted effort toward consensus building.
- To make an RFC, just copy the [0000-template.md](http://0000-template.md/) to [0000-shortname.md](http://0000-shortname.md/). It's also a good idea to submit drafts of RFCs as PRs for greater visibility.
- When you are ready to submit your RFC, create a PR if it hasn't already been done (make sure to remove the [DRAFT] in the title) and request a reviews from the appropriate team or team members.
- Discuss and incorporate feedback - This is an active process, with the goal to resolve and RFC withing a week after submitting for review, unless substantial changes are required.  The owner / champion should drive this process, making sure to follow up with the team members whose approval is required to get the necessary reviews.  To facilitate this process:
    - Make sure that the PRs are added to the appropriate team board either in the discuss column or the review column so outstanding RFCs can be highlighted during the team meetings
    - Add it to the list to be discussed at the engineering all hands
    - If there is a lot of active discussion or not enough discussion, call a meeting with the goal of making a decision. If there’s little or no concern then reviewers should go ahead and approve. Then move forward with the proposed solution.

**RFC Life Cycle**

- Draft - Started but not yet ready for review.  Marked with [DRAFT] in title.
- Open - Ready for review with active discussion and incorporation of feedback.
- Merged - When feedback has been addressed and the feature is ready to be implemented the RFC can be merged.  If there are outstanding questions make sure to capture them in the RFC as well.
- Deprioritzed RFCs - Mark them as postponed as close them. Reopen when they are relevant again.  Mark with the label "Postponed RFC"
- Abandoned RFCs - RFCs that we decide not to do should be closed.

**CleanUp Stale RFCs**

As part of exiting taskforce, have a effort to close out stale RFCs either closing them as Postponed/Abandoned or driving them to decision and landing them.   


**Updating RFCs** 

In general an RFC's purpose is to gain consensus on an approach and act as an artifact for technical design decisions.  It is not a living document of how the code works.  Still there may be times when an RFC should be updated

- Addressing open or unresolved questions
- Revisiting and updating the approach if it has been changed between the RFC being merged and the

RFCs should not be updated to reflect actual implementation. That should be captured in docs and done as part of the development process (as captured in the Definition of Done - TBA)

**The RFC Template**

Suggest adding the following section(s) to the RFC Template:

- Testing - discussing how this feature or changes should be tested and if there's anything (eg. infrastructure) that needs to be done to support that


## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should we RFC substantial changes to our processes as well? Might be good for visibility.
- Should we have a code owners team for the rfc folder. Leaning towards no but worth discussing
