package Model.Communications.FrontEnd;

import com.google.gson.Gson;

import java.util.List;

public class Subscription {

    private final Gson gson;
    private final CommunicationHandler com;

    public Subscription(CommunicationHandler com){
        this.gson = new Gson();
        this.com = com;
    }

    //Método que envia o pedido de subscrição ao servidor front-end
    public void sendSubscriptionReq(String type){
        SubscriptionReqObj tmp = new SubscriptionReqObj(type);
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    //Método que espera pela resposta do servidor front-end aṕos ser feito o pedido de subscrição
    public boolean receiveSubscriptionRes(){
        String json = com.receiveResponse();
        SubscriptionResObj res = gson.fromJson(json, SubscriptionResObj.class);
        return (res.code.equals("200") && res.version.equals("1.0.0") && res.ReplyType.equals("Subscribe"));
    }

    public void sendSubscriptionsReq(){
        SubscriptionsReqObj tmp = new SubscriptionsReqObj();
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    public List<String> receiveSubscriptionsReq(){
        String json = com.receiveResponse();
        SubscriptionsResObj res = gson.fromJson(json,SubscriptionsResObj.class);
        return res.Subscriptions;
    }

    public void sendUnsubscriptionReq(String type){
        UnsubscriptionReqObj tmp = new UnsubscriptionReqObj(type);
        String json = gson.toJson(tmp);
        com.sendRequest(json);
    }

    public boolean receiveUnsubscriptioneRes(){
        String json = com.receiveResponse();
        UnsubscriptionResObj res = gson.fromJson(json, UnsubscriptionResObj.class);
        return (res.code.equals("200") && res.version.equals("1.0.0") && res.ReplyType.equals("Unsubscribe"));
    }

    private class SubscriptionReqObj{
        private final String version = "1.0.0";
        private final String RequestType = "Subscribe";
        private final String SubscriptionType;

        public SubscriptionReqObj(String type){
            this.SubscriptionType = type;
        }
    }

    private class SubscriptionResObj{
        public String version;
        public String ReplyType;
        public String code;
    }

    private class UnsubscriptionReqObj{
        private final String version = "1.0.0";
        private final String RequestType = "Subscribe";
        private final String SubscriptionType;

        public UnsubscriptionReqObj(String type){
            this.SubscriptionType = type;
        }
    }

    private class UnsubscriptionResObj{
        public String version;
        public String ReplyType;
        public String code;
    }

    private class SubscriptionsReqObj{
        private final String version = "1.0.0";
        private final String RequestType = "GetSubscriptions";
    }

    private class SubscriptionsResObj{
        public String version;
        public String ReplyType;
        public List<String> Subscriptions;
    }


}
