/**
 * Cloud Function that disables billing when budget is exceeded.
 *
 * Triggered by a Pub/Sub message from a GCP budget alert.
 * When cost exceeds the budget amount, disconnects the billing account
 * from the project, effectively shutting down all paid services.
 *
 * Setup:
 * 1. Create Pub/Sub topic: gcloud pubsub topics create billing-alerts
 * 2. Create budget in GCP Console > Billing > Budgets & alerts
 *    - Set amount (e.g. $5)
 *    - Connect to Pub/Sub topic "billing-alerts"
 * 3. Grant the Cloud Functions service account billing.projectManager
 *    on the billing account:
 *    gcloud billing accounts add-iam-policy-binding BILLING_ACCOUNT_ID \
 *      --member="serviceAccount:PROJECT_ID@appspot.gserviceaccount.com" \
 *      --role="roles/billing.projectManager"
 * 4. Deploy: firebase deploy --only functions:stopBilling
 */

const { CloudBillingClient } = require("@google-cloud/billing");
const functions = require("firebase-functions");

const PROJECT_ID = process.env.GCLOUD_PROJECT || process.env.GOOGLE_CLOUD_PROJECT;
const PROJECT_NAME = `projects/${PROJECT_ID}`;

const billingClient = new CloudBillingClient();

exports.stopBilling = functions
  .region("europe-west1")
  .pubsub.topic("billing-alerts")
  .onPublish(async (message) => {
    const data = JSON.parse(Buffer.from(message.data, "base64").toString());

    console.log(
      `Budget alert: cost=${data.costAmount}, budget=${data.budgetAmount}, ` +
        `threshold=${data.alertThresholdExceeded}`
    );

    if (data.costAmount <= data.budgetAmount) {
      console.log(
        `Cost (${data.costAmount}) within budget (${data.budgetAmount}). No action.`
      );
      return null;
    }

    console.log(`Cost EXCEEDS budget. Disabling billing for ${PROJECT_NAME}.`);

    const [billingInfo] = await billingClient.getProjectBillingInfo({
      name: PROJECT_NAME,
    });

    if (!billingInfo.billingEnabled) {
      console.log("Billing already disabled.");
      return null;
    }

    const [updated] = await billingClient.updateProjectBillingInfo({
      name: PROJECT_NAME,
      projectBillingInfo: {
        billingAccountName: "", // disconnects billing
      },
    });

    console.log(`Billing disabled. billingEnabled=${updated.billingEnabled}`);
    return null;
  });
